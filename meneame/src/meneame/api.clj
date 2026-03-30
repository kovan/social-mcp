(ns meneame.api
  "Menéame API client. Uses /api/list.php for reading, web forms for writing."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [meneame.cookies :as cookies])
  (:import [java.net URLEncoder]
           [org.jsoup Jsoup]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(def ^:private cookie-jar (atom nil))

(defn- init-cookies! []
  (when-not @cookie-jar
    (let [cks (cookies/get-cookies ".meneame.net")
          f (java.io.File/createTempFile "meneame-cookies" ".txt")]
      (.deleteOnExit f)
      (with-open [w (io/writer f)]
        (.write w "# Netscape HTTP Cookie File\n")
        (doseq [{:keys [name value path host]} cks]
          (when (seq value)
            (.write w (str host "\t"
                          (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                          (or path "/") "\t"
                          "TRUE\t"
                          "0\t"
                          name "\t"
                          value "\n")))))
      (reset! cookie-jar (.getAbsolutePath f))
      (binding [*out* *err*]
        (println (str "Loaded " (count cks) " cookies for meneame.net"))))))

;; --- Public JSON API (no auth needed) ---

(defn- fetch-json [url]
  (let [pb (ProcessBuilder. ["curl" "-sSL"
                              "-H" (str "User-Agent: " ua)
                              url])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn list-stories
  "Fetch stories from Menéame. Options: :status, :rows, :popular, :active, :sub, :q"
  [& {:keys [status rows popular active sub q top_visited sent_by]
      :or {rows 25}}]
  (let [params (cond-> [(str "rows=" rows)]
                 status (conj (str "status=" status))
                 popular (conj "popular")
                 active (conj "active")
                 top_visited (conj "top_visited")
                 sub (conj (str "sub=" (URLEncoder/encode sub "UTF-8")))
                 q (conj (str "q=" (URLEncoder/encode q "UTF-8")))
                 sent_by (conj (str "sent_by=" (URLEncoder/encode sent_by "UTF-8"))))
        url (str "https://www.meneame.net/api/list.php?" (str/join "&" params))]
    (fetch-json url)))

(defn get-comments
  "Fetch comments for a story by link ID."
  [link-id]
  (fetch-json (str "https://www.meneame.net/api/list.php?id=" link-id)))

;; --- Authenticated web interactions ---

(defn- curl-authed [url & {:keys [method params]}]
  (init-cookies!)
  (let [body-file (java.io.File/createTempFile "mn-resp" ".html")
        args (cond-> ["curl" "-sSL"
                      "-b" @cookie-jar "-c" @cookie-jar
                      "-H" (str "User-Agent: " ua)
                      "-o" (.getAbsolutePath body-file)
                      "-w" "%{http_code}"]
               (= method :post)
               (into ["-X" "POST"])
               (seq params)
               (into ["-d" (str/join "&"
                             (map (fn [[k v]]
                                    (str (URLEncoder/encode (name k) "UTF-8") "="
                                         (URLEncoder/encode (str v) "UTF-8")))
                                  params))])
               true
               (conj url))
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    {:status (or (parse-long status-str) 0) :body body}))

(defn post-comment
  "Post a comment on a story. Requires being logged in."
  [link-id text & {:keys [parent-id]}]
  ;; First fetch the story page to get any tokens
  (let [story-resp (curl-authed (str "https://www.meneame.net/story/" link-id))
        doc (Jsoup/parse ^String (:body story-resp))
        ;; Find the comment form
        form (.selectFirst doc "form.commentform")
        _ (when-not form
            (throw (ex-info "Comment form not found - make sure you're logged in to Menéame in Chrome" {})))
        ;; Extract hidden fields
        hidden-inputs (.select form "input[type=hidden]")
        form-params (into {} (for [input hidden-inputs
                                   :let [n (.attr input "name")
                                         v (.attr input "value")]
                                   :when (seq n)]
                               [n v]))
        ;; Add our comment
        post-params (merge form-params
                           {"comment_content" text
                            "process" "newcomment"}
                           (when parent-id
                             {"parent_id" (str parent-id)}))
        post-resp (curl-authed (str "https://www.meneame.net/story/" link-id)
                    :method :post
                    :params post-params)]
    (if (<= 200 (:status post-resp) 303)
      (str "Comment posted successfully on story " link-id ".")
      (throw (ex-info (str "Comment failed: HTTP " (:status post-resp)) {})))))
