(ns clarin.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader StringWriter PrintWriter])
  (:gen-class))

;;; ---- Viafoura API config ----

(def ^:private section-uuid "00000000-0000-4000-8000-82628f44cd3d")
(def ^:private viafoura-api "https://livecomments.viafoura.co/v4/livecomments")
(def ^:private base-url "https://www.clarin.com")
(def ^:private ua "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

;;; ---- Auth (handled by Playwright in post_comment.py) ----

;;; ---- HTTP helpers ----

(defn- curl-get [url & {:keys [token]}]
  (let [args (cond-> ["curl" "-sS" "--max-time" "20"
                       "-H" (str "User-Agent: " ua)]
               token (into ["-H" (str "Authorization: Bearer " token)])
               true  (conj url))
        pb (ProcessBuilder. ^java.util.List (vec args))
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- curl-post [url body & {:keys [token]}]
  (let [json-str (json/write-str body)
        args (cond-> ["curl" "-sS" "--max-time" "20"
                       "-X" "POST"
                       "-H" "Content-Type: application/json"
                       "-H" (str "User-Agent: " ua)]
               token (into ["-H" (str "Authorization: Bearer " token)])
               true  (into ["-d" json-str url]))
        pb (ProcessBuilder. ^java.util.List (vec args))
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        _ (.waitFor proc)]
    (when-not (str/blank? err)
      (binding [*out* *err*]
        (println "curl stderr:" err)))
    (when (seq out)
      (try (json/read-str out :key-fn keyword)
           (catch Exception _ {:raw out})))))

(defn- curl-html [url]
  (let [pb (ProcessBuilder. ["curl" "-sS" "--max-time" "15" "-A" ua url])
        proc (.start pb)
        html (slurp (.getInputStream proc))
        _ (.waitFor proc)]
    html))

;;; ---- Article ID + Viafoura container ----

(defn- extract-article-id
  "Extract the short article ID from a Clarin URL.
   URLs look like: /seccion/slug_0_ARTICLE_ID.html or /seccion/slug_5_ARTICLE_ID.html
   The article ID is the alphanumeric part after the last underscore-digit-underscore."
  [article-url]
  (let [path (-> (java.net.URI. article-url) .getPath)
        filename (last (str/split path #"/"))
        slug (when filename (str/replace filename #"\.html$" ""))
        ;; Extract short ID: after _0_ or _5_ etc
        m (when slug (re-find #"_\d+_([A-Za-z0-9]+)$" slug))]
    (if m
      (second m)
      ;; Fallback: use the whole slug
      slug)))

(defn- get-container-uuid
  "Get the Viafoura content_container_uuid for an article ID."
  [article-id]
  (let [url (str base-url "/api/viafoura/comments/" article-id)
        resp (curl-get url)]
    (when (and resp (= 200 (:code resp)))
      {:container-uuid (get-in resp [:data :content_container_uuid])
       :comment-count  (get-in resp [:data :total_visible_content])})))

;;; ---- Front page ----

(defn- api-front-page []
  (let [html (curl-html base-url)
        ;; Clarin article URLs: /seccion/slug.html or /seccion/sub/slug.html
        url-pat (java.util.regex.Pattern/compile
                  "href=\"(/[a-z][a-z0-9-]*/[^\"]+\\.html)\"")
        m (.matcher url-pat html)
        urls (loop [acc [] seen #{}]
               (if (.find m)
                 (let [path (.group m 1)
                       full (str base-url path)]
                   (if (or (contains? seen full)
                           (str/includes? path "/auth/")
                           (str/includes? path "/ayuda/")
                           (str/includes? path "/servicios/")
                           (str/includes? path "/suscripciones/")
                           (str/includes? path "/tema/")
                           (str/includes? path "/estadisticas/"))
                     (recur acc seen)
                     (recur (conj acc full) (conj seen full))))
                 acc))]
    urls))

;;; ---- Read comments ----

(defn- relative-time [iso-str]
  (when (seq iso-str)
    (try
      (let [inst (java.time.Instant/parse iso-str)
            now (java.time.Instant/now)
            secs (.between java.time.temporal.ChronoUnit/SECONDS inst now)]
        (cond
          (< secs 60) "just now"
          (< secs 3600) (str (quot secs 60) "m ago")
          (< secs 86400) (str (quot secs 3600) "h ago")
          :else (str (quot secs 86400) "d ago")))
      (catch Exception _ iso-str))))

(defn- format-comment [c depth]
  (let [prefix (apply str (repeat depth "  "))
        ;; Viafoura uses actor_uuid, not nested author object.
        ;; Display name may come from a separate user lookup; for now use actor_uuid short form.
        author (or (:display_name c)
                   (let [uuid (:actor_uuid c)]
                     (when (and uuid (> (count uuid) 12))
                       (subs uuid (- (count uuid) 12)))))
        text (or (:content c) "")
        time-str (relative-time
                   (when-let [ts (:date_created c)]
                     ;; Viafoura returns epoch millis, convert to ISO
                     (str (java.time.Instant/ofEpochMilli (long ts)))))
        uuid (:content_uuid c)
        likes (:total_likes c 0)
        dislikes (:total_dislikes c 0)
        replies (:contents (:replies c) [])]
    (str prefix (or author "anonymous") " [+" likes "/-" dislikes "]"
         (when time-str (str " - " time-str))
         "\n" prefix text
         "\n" prefix "[id:" uuid "]"
         (when (seq replies)
           (str "\n" (str/join "\n\n"
                       (map #(format-comment % (inc depth)) replies)))))))

(defn- api-read-comments [article-url n]
  (let [article-id (extract-article-id article-url)
        _ (when-not article-id (throw (ex-info "Cannot extract article ID from URL" {:url article-url})))
        {:keys [container-uuid comment-count]} (get-container-uuid article-id)
        _ (when-not container-uuid
            (throw (ex-info "Cannot find Viafoura container for article" {:article-id article-id})))
        limit (min (max (or n 20) 1) 100)
        url (str viafoura-api "/" section-uuid "/" container-uuid
                 "/comments?limit=" limit "&reply_limit=3&sorted_by=newest")
        resp (curl-get url)
        comments (or (:contents resp) [])]
    (if (seq comments)
      (let [title (get-in (first comments) [:metadata :origin_title])]
        (str (or title article-url)
             " (" (or comment-count (count comments)) " comments)\n\n"
             (str/join "\n\n" (map #(format-comment % 0) comments))))
      (str article-url "\nNo comments yet."))))

;;; ---- Post comment (via Playwright) ----

(def ^:private post-script-path
  (str (System/getProperty "user.dir") "/post_comment.py"))

(defn- api-post-comment [article-url body parent-id]
  (let [args-json (json/write-str (cond-> {:url article-url :body body}
                                    parent-id (assoc :parent_id parent-id)))
        pb (ProcessBuilder. ["python3" post-script-path args-json])
        _ (.redirectErrorStream pb true)
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        exit (.waitFor proc 120 java.util.concurrent.TimeUnit/SECONDS)]
    (if (and exit (zero? (.exitValue proc)))
      (let [resp (try (json/read-str out :key-fn keyword) (catch Exception _ nil))]
        (if (and resp (:success resp))
          (str "Comment posted successfully.")
          (str "Post result: " out)))
      (throw (ex-info (str "Playwright post failed: " out) {})))))

;;; ---- MCP boilerplate ----

(def ^:private tools
  [{:name "front_page"
    :description "List today's articles from Clarin.com (Argentina's largest circulation newspaper). Returns titles and URLs."
    :inputSchema {:type "object" :properties {}}}
   {:name "read_comments"
    :description "Read comments on a Clarin article via Viafoura. Returns comment text, usernames, and IDs (needed for replies)."
    :inputSchema {:type "object"
                  :properties {:article_url {:type "string"
                                             :description "Full URL of the Clarin article"}
                               :n {:type "number"
                                   :description "Number of comments to fetch (default 20, max 100)"}}
                  :required ["article_url"]}}
   {:name "post_comment"
    :description "Post a comment on a Clarin article. Uses Chrome cookies for auth - must be logged in to clarin.com in Chrome."
    :inputSchema {:type "object"
                  :properties {:article_url {:type "string"
                                             :description "Full URL of the Clarin article to comment on"}
                               :body {:type "string"
                                      :description "Comment text"}
                               :parent_id {:type "string"
                                           :description "UUID of comment to reply to (optional, for threaded replies)"}}
                  :required ["article_url" "body"]}}])

(defn- respond [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn- error-response [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn- tool-result [text & {:keys [error?]}]
  {:content [{:type "text" :text text}]
   :isError (boolean error?)})

(defn- handle-initialize [id _params]
  (respond id
    {:protocolVersion "2024-11-05"
     :capabilities {:tools {}}
     :serverInfo {:name "clarin-mcp" :version "0.1.0"}
     :instructions "MCP server for Clarin.com (Argentina). Uses Chrome cookies for authentication - make sure you're logged in to clarin.com in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- clamp [n default mx]
  (min (max (or (when n (long n)) default) 1) mx))

(defn- format-front-page [urls]
  (if (empty? urls)
    "No articles found."
    (str "# Clarin.com - Front page (" (count urls) " articles)\n\n"
         (->> urls
              (map-indexed (fn [i url]
                             (let [slug (-> url
                                           (str/replace (str base-url "/") "")
                                           (str/replace #"\.html$" "")
                                           (str/replace #"/" " > ")
                                           (str/replace #"-" " "))]
                               (str (inc i) ". " slug "\n   " url))))
              (str/join "\n")))))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "front_page"
            (format-front-page (api-front-page))

            "read_comments"
            (api-read-comments (:article_url arguments)
                               (clamp (:n arguments) 20 100))

            "post_comment"
            (api-post-comment (:article_url arguments)
                              (:body arguments)
                              (:parent_id arguments))

            (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (let [sw (StringWriter.)
            pw (PrintWriter. sw)]
        (.printStackTrace e pw)
        (respond id (tool-result (str "Error: " (.getMessage e) "\n\n" (.toString sw))
                                 :error? true))))))

(defn- handle-message [msg]
  (let [{:keys [id method params]} msg]
    (case method
      "initialize"                (handle-initialize id params)
      "notifications/initialized" nil
      "tools/list"                (handle-tools-list id params)
      "tools/call"                (handle-tools-call id params)
      "ping"                      (respond id {})
      (if id
        (error-response id -32601 (str "Method not found: " method))
        nil))))

(defn- write-response [resp]
  (let [out System/out
        line (str (json/write-str resp) "\n")]
    (.write out (.getBytes line "UTF-8"))
    (.flush out)))

(defn -main [& _args]
  (let [reader (BufferedReader. (InputStreamReader. System/in))]
    (loop []
      (when-let [line (.readLine reader)]
        (when-not (str/blank? line)
          (try
            (let [msg (json/read-str line :key-fn keyword)
                  resp (handle-message msg)]
              (when resp
                (write-response resp)))
            (catch Exception e
              (binding [*out* *err*]
                (println "Error processing message:" (.getMessage e))))))
        (recur)))))
