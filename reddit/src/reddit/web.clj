(ns reddit.web
  "Authenticated web interactions with Reddit (comment, submit, vote)."
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [reddit.cookies :as cookies])
  (:import [java.net URLEncoder]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(def ^:private cookie-jar (atom nil))
(def ^:private modhash (atom nil))

(defn- init-cookies! []
  (let [cks (cookies/get-cookies ".reddit.com")
        f (java.io.File/createTempFile "reddit-cookies" ".txt")]
    (.deleteOnExit f)
    (with-open [w (io/writer f)]
      (.write w "# Netscape HTTP Cookie File\n")
      (doseq [{:keys [name value path host]} cks]
        (when (and (seq value) (some? host))
          (.write w (str host "\t"
                         (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                         path "\t"
                         "TRUE\t"
                         "0\t"
                         name "\t"
                         value "\n")))))
    (reset! cookie-jar (.getAbsolutePath f))
    (binding [*out* *err*]
      (println (str "Loaded " (count cks) " cookies for reddit.com")))))

(defn- fetch-modhash! []
  (when-not @modhash
    (init-cookies!)
    (let [pb (ProcessBuilder. ["curl" "-sSL"
                                "-b" @cookie-jar "-c" @cookie-jar
                                "-H" (str "User-Agent: " ua)
                                "https://old.reddit.com/api/me.json"])
          proc (.start pb)
          out (str/trim (slurp (.getInputStream proc)))
          _ (.waitFor proc)
          data (when (seq out) (json/read-str out :key-fn keyword))
          mh (get-in data [:data :modhash])]
      (if (seq mh)
        (do (reset! modhash mh)
            (binding [*out* *err*]
              (println "Got modhash for" (get-in data [:data :name]))))
        (throw (ex-info "Could not get modhash - make sure you're logged in to Reddit in Chrome" {}))))))

(defn- api-post [endpoint params]
  (init-cookies!)
  (fetch-modhash!)
  (let [body-file (java.io.File/createTempFile "reddit-resp" ".json")
        form-data (str/join "&"
                    (map (fn [[k v]]
                           (str (URLEncoder/encode (name k) "UTF-8") "="
                                (URLEncoder/encode (str v) "UTF-8")))
                         (assoc params :uh @modhash :api_type "json")))
        args ["curl" "-sSL"
              "-b" @cookie-jar "-c" @cookie-jar
              "-H" (str "User-Agent: " ua)
              "-X" "POST"
              "-d" form-data
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              (str "https://old.reddit.com/api/" endpoint)]
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "curl error:" err)))
    {:status (or (parse-long status-str) 0)
     :body body
     :data (try (json/read-str body :key-fn keyword) (catch Exception _ nil))}))

(defn- api-get
  "Authenticated GET request to old.reddit.com."
  [url]
  (init-cookies!)
  (let [pb (ProcessBuilder. ["curl" "-sSL"
                              "-b" @cookie-jar "-c" @cookie-jar
                              "-H" (str "User-Agent: " ua)
                              url])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- relative-time [epoch-secs]
  (when epoch-secs
    (let [now (quot (System/currentTimeMillis) 1000)
          diff (- now (long epoch-secs))]
      (cond
        (< diff 60) "just now"
        (< diff 3600) (str (quot diff 60) "m ago")
        (< diff 86400) (str (quot diff 3600) "h ago")
        :else (str (quot diff 86400) "d ago")))))

(defn- format-inbox-item
  [{:keys [kind data]}]
  (let [{:keys [author subject body created_utc context
                subreddit link_title new]} data
        kind-label (case kind "t1" "comment reply" "t4" "message" kind)
        time-str (relative-time created_utc)
        link (when (seq context)
               (str "https://old.reddit.com" context))]
    (str (when new "**[NEW]** ")
         "**" (or author "[deleted]") "** (" kind-label ")"
         (when time-str (str " - " time-str)) "\n"
         (when (and (seq subreddit) (seq link_title))
           (str "r/" subreddit " | " link_title "\n"))
         (when (and (= kind "t4") (seq subject))
           (str "Subject: " subject "\n"))
         (when (seq body) (str body "\n"))
         (when link (str link "\n")))))

(defn inbox
  "Fetch the authenticated user's inbox. Returns formatted markdown."
  [n]
  (let [limit (min (max n 1) 100)
        url (str "https://old.reddit.com/message/inbox/.json?limit=" limit "&raw_json=1")
        response (api-get url)
        items (get-in response [:data :children])]
    (if (seq items)
      (str "# Inbox (" (count items) " items)\n\n"
           (str/join "\n---\n\n"
             (map-indexed
               (fn [i item]
                 (str (inc i) ". " (format-inbox-item item)))
               items)))
      "# Inbox\n\nNo messages.")))

(defn reply-comment
  "Reply to a comment or post on Reddit. thing-id is the fullname (t1_ or t3_ prefixed)."
  [thing-id message]
  (let [resp (api-post "comment" {:thing_id thing-id :text message})
        errors (get-in resp [:data :json :errors])]
    (if (and (seq errors) (not= errors []))
      (throw (ex-info (str "Reddit API error: " (pr-str errors)) {:errors errors}))
      (str "Reply posted successfully to " thing-id "."))))

(defn list-flairs
  "List available post flairs for a subreddit."
  [subreddit]
  (let [url (str "https://old.reddit.com/r/" subreddit "/api/link_flair_v2.json")
        flairs (api-get url)]
    (if (seq flairs)
      (str "# Flairs for r/" subreddit " (" (count flairs) ")\n\n"
           (clojure.string/join "\n"
             (map (fn [{:keys [id text]}]
                    (str "- " text " [" id "]"))
                  flairs)))
      (str "# Flairs for r/" subreddit "\n\nNo flairs found (or sub doesn't require them)."))))

(defn mark-inbox-read
  "Mark inbox messages as read. Pass specific fullnames (e.g. t1_abc,t4_def)
   or nil/empty to mark everything as read."
  [ids]
  (if (seq ids)
    (do (api-post "read_message" {:id ids})
        (str "Marked as read: " ids))
    (do (api-post "read_all_messages" {})
        "Marked all inbox messages as read.")))

(defn my-comments
  "Fetch the current user's recent comments."
  [n]
  (let [limit (min (max n 1) 100)
        url (str "https://old.reddit.com/user/me/comments.json?limit=" limit "&raw_json=1")
        response (api-get url)
        items (get-in response [:data :children])]
    (if (seq items)
      (str "# My recent comments (" (count items) ")\n\n"
           (str/join "\n---\n\n"
             (map-indexed
               (fn [i {:keys [data]}]
                 (let [{:keys [body subreddit link_title permalink
                               created_utc score name]} data]
                   (str (inc i) ". **r/" subreddit "** | " link_title "\n"
                        (relative-time created_utc) " | " score " pts | [" name "]\n"
                        "https://www.reddit.com" permalink "\n"
                        (when (seq body)
                          (str (subs body 0 (min 300 (count body)))
                               (when (> (count body) 300) "..."))))))
               items)))
      "# My recent comments\n\nNo comments found.")))

(defn submit-post
  "Submit a new post to a subreddit. kind is 'self' or 'link'."
  [subreddit title kind & {:keys [text url flair-id]}]
  (let [params (cond-> {:sr subreddit :title title :kind kind}
                 text (assoc :text text)
                 url (assoc :url url)
                 flair-id (assoc :flair_id flair-id))
        resp (api-post "submit" params)
        errors (get-in resp [:data :json :errors])]
    (if (and (seq errors) (not= errors []))
      (throw (ex-info (str "Reddit API error: " (pr-str errors)) {:errors errors}))
      (let [post-url (get-in resp [:data :json :data :url])]
        (str "Post submitted successfully."
             (when post-url (str " URL: " post-url)))))))
