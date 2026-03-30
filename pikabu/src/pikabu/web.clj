(ns pikabu.web
  "Authenticated web interactions with Pikabu (comment, vote)."
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [pikabu.cookies :as cookies])
  (:import [java.net URLEncoder]))

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
    (let [pb (ProcessBuilder. ["curl" "-sSL"
                                "-b" @cookie-jar "-c" @cookie-jar
                                "-H" (str "User-Agent: " ua)
                                "https://pikabu.ru/"])
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
        args ["curl" "-sSL"
              "-b" @cookie-jar "-c" @cookie-jar
              "-H" (str "User-Agent: " ua)
              "-H" "X-Requested-With: XMLHttpRequest"
              "-X" "POST"
              "-d" form-data
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              url]
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

(defn post-comment
  "Post a comment on a Pikabu story."
  [story-id text & {:keys [parent-id]}]
  (let [params {:action "create"
                :story_id story-id
                :parent_id (or parent-id "0")
                :desc text}
        resp (api-post "https://pikabu.ru/ajax/comments_actions.php" params)]
    (if (get-in resp [:data :result])
      (let [cid (get-in resp [:data :comment_id])]
        (str "Comment posted on story " story-id ". Comment ID: " cid))
      (let [msg (or (get-in resp [:data :message]) (:body resp))]
        (throw (ex-info (str "Failed to post comment: " msg) {:response resp}))))))

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
