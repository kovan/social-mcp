(ns bluesky.api
  "AT Protocol client for Bluesky (bsky.social)."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URLEncoder]))

(def ^:private base-url "https://bsky.social/xrpc")
(def ^:private public-base-url "https://api.bsky.app/xrpc")
(def ^:private discover-feed-uri
  "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot")
(def ^:private creds-path
  (str (System/getProperty "user.home") "/.config/social-mcp/bluesky.edn"))

(def ^:private session (atom nil))

(defn- read-creds-file []
  (let [f (io/file creds-path)]
    (when (.exists f)
      (let [data (edn/read-string (slurp f))
            handle (or (:handle data) (:identifier data))
            password (or (:app-password data) (:password data))]
        (when (and (seq handle) (seq password))
          {:handle handle :password password})))))

(defn- resolve-creds []
  (let [env-handle (System/getenv "BLUESKY_HANDLE")
        env-password (System/getenv "BLUESKY_APP_PASSWORD")]
    (cond
      (and (seq env-handle) (seq env-password))
      {:handle env-handle :password env-password}

      :else
      (read-creds-file))))

(defn- http-post [url body & {:keys [token]}]
  (let [json-str (json/write-str body)
        args (cond-> ["curl" "-sS" "-X" "POST"
                      "-H" "Content-Type: application/json"
                      "-H" "Accept: application/json"]
               token (into ["-H" (str "Authorization: Bearer " token)])
               true (into ["-d" json-str url]))
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- http-get [url & {:keys [token params]}]
  (let [query-str (when (seq params)
                    (str "?" (str/join "&"
                              (map (fn [[k v]]
                                     (str (URLEncoder/encode (name k) "UTF-8") "="
                                          (URLEncoder/encode (str v) "UTF-8")))
                                   params))))
        full-url (str url query-str)
        args (cond-> ["curl" "-sSL"
                      "-H" "Accept: application/json"]
               token (into ["-H" (str "Authorization: Bearer " token)])
               true (conj full-url))
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- has-creds? []
  (boolean (resolve-creds)))

(defn read-only-mode?
  []
  (not (has-creds?)))

(defn- get-creds []
  (or (resolve-creds)
      (throw (ex-info (str "Set BLUESKY_HANDLE and BLUESKY_APP_PASSWORD, "
                           "or create " creds-path)
                      {}))))

(defn- ensure-session! []
  (when-not @session
    (let [{:keys [handle password]} (get-creds)
          resp (http-post (str base-url "/com.atproto.server.createSession")
                          {:identifier handle :password password})]
      (if (:accessJwt resp)
        (do (reset! session resp)
            (binding [*out* *err*]
              (println "Logged in to Bluesky as" (:handle resp))))
        (throw (ex-info (str "Bluesky auth failed: " (or (:message resp) (pr-str resp))) {})))))
  @session)

(defn- tok [] (:accessJwt (ensure-session!)))
(defn- my-did [] (:did (ensure-session!)))

(defn- url->at-uri
  "Convert bsky.app URL to AT URI. Returns as-is if already at://."
  [uri-or-url]
  (if (str/starts-with? uri-or-url "at://")
    uri-or-url
    (if-let [[_ handle rkey] (re-find #"bsky\.app/profile/([^/]+)/post/([^/?#]+)" uri-or-url)]
      (let [profile (http-get (str public-base-url "/app.bsky.actor.getProfile")
                              :params {:actor handle})]
        (str "at://" (:did profile) "/app.bsky.feed.post/" rkey))
      (throw (ex-info (str "Cannot parse URI/URL: " uri-or-url) {})))))

(defn timeline
  "Get authenticated user's home timeline, or the public Discover feed if no credentials are configured."
  [& {:keys [limit] :or {limit 25}}]
  (if (has-creds?)
    (http-get (str base-url "/app.bsky.feed.getTimeline")
              :token (tok) :params {:limit (min limit 100)})
    (http-get (str public-base-url "/app.bsky.feed.getFeed")
              :params {:feed discover-feed-uri
                       :limit (min limit 100)})))

(defn search
  "Search Bluesky posts. Normalizes response to {:feed [...]} format."
  [query & {:keys [limit sort] :or {limit 25 sort "latest"}}]
  (let [resp (http-get (str (if (has-creds?) base-url public-base-url) "/app.bsky.feed.searchPosts")
                       :token (when (has-creds?) (tok))
                       :params (cond-> {:q query :limit (min limit 100)}
                                sort (assoc :sort sort)))]
    {:feed (mapv #(hash-map :post %) (:posts resp))}))

(defn get-thread
  "Get a post and its reply thread."
  [uri-or-url & {:keys [depth] :or {depth 6}}]
  (let [uri (url->at-uri uri-or-url)]
    (http-get (str (if (has-creds?) base-url public-base-url) "/app.bsky.feed.getPostThread")
              :token (when (has-creds?) (tok))
              :params {:uri uri :depth depth})))

(defn notifications
  "Get notifications for the authenticated user."
  [& {:keys [limit] :or {limit 25}}]
  (http-get (str base-url "/app.bsky.notification.listNotifications")
            :token (tok) :params {:limit (min limit 50)}))

(defn post
  "Create a new Bluesky post."
  [text]
  (http-post (str base-url "/com.atproto.repo.createRecord")
             {:repo (my-did)
              :collection "app.bsky.feed.post"
              :record {:$type "app.bsky.feed.post"
                       :text text
                       :createdAt (str (java.time.Instant/now))}}
             :token (tok)))

(defn follow
  "Follow a Bluesky user by handle (e.g. user.bsky.social)."
  [handle]
  (let [h (str/replace handle #"^@" "")
        profile (http-get (str base-url "/app.bsky.actor.getProfile")
                          :token (tok) :params {:actor h})
        _ (when-not (:did profile)
            (throw (ex-info (str "User not found: " h) {})))
        did-str (:did profile)]
    (http-post (str base-url "/com.atproto.repo.createRecord")
               {:repo (my-did)
                :collection "app.bsky.graph.follow"
                :record {:$type "app.bsky.graph.follow"
                         :subject did-str
                         :createdAt (str (java.time.Instant/now))}}
               :token (tok))
    (str "Now following @" h
         (when-let [n (:displayName profile)] (str " (" n ")")))))

(defn reply
  "Reply to a post. Accepts AT URI or bsky.app URL. Auto-fetches CID."
  [uri-or-url text]
  (let [uri (url->at-uri uri-or-url)
        thread (http-get (str base-url "/app.bsky.feed.getPostThread")
                         :token (tok) :params {:uri uri :depth 0})
        post-data (get-in thread [:thread :post])
        _ (when-not post-data
            (throw (ex-info (str "Post not found: " uri) {})))
        cid (:cid post-data)
        reply-ref (if-let [existing-reply (get-in post-data [:record :reply])]
                    {:root (:root existing-reply)
                     :parent {:uri uri :cid cid}}
                    {:root {:uri uri :cid cid}
                     :parent {:uri uri :cid cid}})]
    (http-post (str base-url "/com.atproto.repo.createRecord")
               {:repo (my-did)
                :collection "app.bsky.feed.post"
                :record {:$type "app.bsky.feed.post"
                         :text text
                         :createdAt (str (java.time.Instant/now))
                         :reply reply-ref}}
               :token (tok))))
