(ns pikabu.web
  "Authenticated web interactions with Pikabu (comment, vote, notifications)."
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [pikabu.cookies :as cookies]
            [pikabu.proxy :as px])
  (:import [java.net URLEncoder]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]
           [org.jsoup.select Elements]))

(def ^:private ua
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36")

(def ^:private cookie-jar (atom nil))
(def ^:private session-initialized (atom false))

(defn- init-cookies! []
  (when-not @cookie-jar
    (let [cks (cookies/get-cookies ".pikabu.ru")
          f (java.io.File/createTempFile "pikabu-cookies" ".txt")]
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
        (println (str "Loaded " (count cks) " cookies for pikabu.ru"))))))

(defn- ensure-session! []
  "Visit main page to establish session cookies before making API calls."
  (init-cookies!)
  (when-not @session-initialized
    (let [args (px/curl-args ["-sSL"
                              "-b" @cookie-jar "-c" @cookie-jar
                              "-H" (str "User-Agent: " ua)
                              "https://pikabu.ru/"])
          pb (ProcessBuilder. ^java.util.List args)
          proc (.start pb)]
      (slurp (.getInputStream proc))
      (.waitFor proc)
      (reset! session-initialized true)
      (binding [*out* *err*]
        (println "Session initialized via main page")))))

(defn- api-post
  "Make an authenticated POST request to Pikabu."
  [url form-params]
  (ensure-session!)
  (let [body-file (java.io.File/createTempFile "pikabu-resp" ".json")
        form-data (str/join "&"
                    (map (fn [[k v]]
                           (str (URLEncoder/encode (name k) "UTF-8") "="
                                (URLEncoder/encode (str v) "UTF-8")))
                         form-params))
        args (px/curl-args ["-sSL"
              "-b" @cookie-jar "-c" @cookie-jar
              "-H" (str "User-Agent: " ua)
              "-H" "X-Requested-With: XMLHttpRequest"
              "-X" "POST"
              "-d" form-data
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              url])
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
  "Make an authenticated GET request to Pikabu. Returns {:bytes raw-bytes :status http-code}."
  [url]
  (ensure-session!)
  (let [out-file (java.io.File/createTempFile "pikabu-get" ".html")
        args (px/curl-args ["-sSL"
              "-b" @cookie-jar "-c" @cookie-jar
              "-H" (str "User-Agent: " ua)
              "-o" (.getAbsolutePath out-file)
              "-w" "%{http_code}"
              url])
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)
        out-bytes (.readAllBytes (java.io.FileInputStream. out-file))
        status (or (parse-long status-str) 0)]
    (.delete out-file)
    {:bytes out-bytes :status status}))

(defn- parse-html-bytes
  "Parse HTML bytes as windows-1251 into a Jsoup Document."
  ^Document [^bytes html-bytes ^String base-uri]
  (Jsoup/parse (java.io.ByteArrayInputStream. html-bytes) "windows-1251" base-uri))

(defn- parse-answer-html
  "Parse an answer HTML block (containing our comment + reply) into a formatted string."
  [idx html-str story-url]
  (let [doc (Jsoup/parse html-str)
        comments (.select doc ".comment")
        nicks (mapv #(when-let [el (.selectFirst ^Element % ".user__nick")]
                       (str/trim (.text el)))
                    comments)
        texts (mapv #(when-let [el (.selectFirst ^Element % ".comment__content")]
                       (str/trim (.text el)))
                    comments)
        reply-id (when (>= (.size comments) 2)
                   (.attr ^Element (.get comments 1) "data-id"))
        our-text (first texts)
        reply-nick (second nicks)
        reply-text (second texts)
        our-text-short (when (seq our-text)
                         (if (> (count our-text) 100)
                           (str (subs our-text 0 100) "...")
                           our-text))]
    (str (inc idx) ". **" (or reply-nick "?") "** replied to you"
         (when story-url (str " on " story-url))
         "\n   reply_id=" (or reply-id "?")
         "\n   > " (or our-text-short "[your comment]")
         "\n   " (or reply-text "[reply text]"))))

(defn notifications
  "Fetch replies to our comments via /answers endpoint.
   Page 1: GET /answers (HTML, includes bell count).
   Page 2+: POST /answers with page param (JSON API, supports infinite scroll)."
  [& {:keys [page] :or {page 1}}]
  (if (= page 1)
    ;; Page 1: GET HTML (includes bell count)
    (let [{:keys [bytes status]} (api-get "https://pikabu.ru/answers")]
      (when (not= status 200)
        (throw (ex-info (str "HTTP " status " from Pikabu (rate-limited? try again later)") {})))
      (let [doc (parse-html-bytes bytes "https://pikabu.ru/answers")
            bell (.selectFirst doc ".bell[data-role=answers]")
            unread-count (when bell (str/trim (.text bell)))
            containers (.select doc ".comments[data-story-url]")
            pairs (for [^Element c containers]
                    {:html (.outerHtml c)
                     :story-url (.attr c "data-story-url")})]
        (if (empty? pairs)
          (str "No replies found." (when unread-count (str " (Bell shows: " unread-count ")")))
          (let [formatted (map-indexed
                            (fn [i {:keys [html story-url]}]
                              (parse-answer-html i html story-url))
                            pairs)]
            (str "# Replies (" (count pairs) " answers"
                 (when unread-count (str ", bell: " unread-count))
                 ")\n\n"
                 (str/join "\n\n---\n\n" formatted))))))
    ;; Page 2+: POST JSON API (infinite scroll)
    (let [resp (api-post "https://pikabu.ru/answers"
                         {:page page :twitmode 1 :of "v2"})]
      (if-not (get-in resp [:data :result])
        (throw (ex-info (str "Answers API error: " (get-in resp [:data :message])) {}))
        (let [comments (get-in resp [:data :data :comments])
              has-more (get-in resp [:data :data :has_more])]
          (if (empty? comments)
            (str "No more replies (page " page ").")
            (let [formatted (map-indexed
                              (fn [i item]
                                (let [html (or (:html item) "")
                                      story-url (second (re-find #"data-story-url=\"([^\"]+)\"" html))]
                                  (parse-answer-html i html story-url)))
                              comments)]
              (str "# Replies (" (count comments) " answers, page " page
                   (when has-more ", has_more")
                   ")\n\n"
                   (str/join "\n\n---\n\n" formatted)))))))))

(defn post-comment
  "Post a comment on a Pikabu story."
  [story-id text & {:keys [parent-id]}]
  (let [params {:action "create"
                :story_id story-id
                :parent_id (or parent-id "0")
                :desc text}
        resp (api-post "https://pikabu.ru/ajax/comments_actions.php" params)]
    (if (get-in resp [:data :result])
      (let [cid (or (get-in resp [:data :comment_id])
                     (get-in resp [:data :id])
                     (get-in resp [:data :data :comment_id])
                     (get-in resp [:data :data :id]))]
        (str "Comment posted on story " story-id ". Comment ID: " cid))
      (let [msg (or (get-in resp [:data :message]) (:body resp))]
        (throw (ex-info (str "Failed to post comment: " msg) {:response resp}))))))

(defn mark-answers-read
  "Clear the answers bell by marking all replies as read."
  []
  (let [resp (api-post "https://pikabu.ru/ajax/comments_actions.php"
                       {:action "mark_readed_all"})]
    (if (get-in resp [:data :result])
      "All answers marked as read."
      (let [msg (or (get-in resp [:data :message]) (:body resp))]
        (throw (ex-info (str "Failed to mark read: " msg) {:response resp}))))))

(defn vote-story
  "Vote on a Pikabu story. direction is 1 (up) or -1 (down)."
  [story-id direction]
  (let [resp (api-post "https://pikabu.ru/ajax/vote.php"
                        {:story_id story-id
                         :vote (if (pos? direction) "1" "-1")})]
    (if (get-in resp [:data :result])
      (str "Voted " (if (pos? direction) "up" "down") " on story " story-id ".")
      (let [msg (or (get-in resp [:data :message]) (:body resp))]
        (throw (ex-info (str "Vote failed: " msg) {:response resp}))))))
