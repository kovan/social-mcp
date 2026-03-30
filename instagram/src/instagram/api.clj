(ns instagram.api
  "Instagram API via instagrapi Python subprocess."
  (:require [clojure.string :as str]
            [clojure.data.json :as json]))

(def ^:private script-path
  (str (System/getProperty "user.home") "/social-mcp/instagram/instagram_api.py"))

(defn- run-python [cmd args]
  (let [pb (ProcessBuilder. ["python3" script-path cmd (json/write-str args)])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)]
    (when (pos? exit)
      (let [err-msg (try (get (json/read-str err :key-fn keyword) :error err)
                         (catch Exception _ err))]
        (throw (ex-info (str "Instagram API error: " err-msg) {:exit exit}))))
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn hashtag-top [hashtag n]
  (run-python "hashtag_top" {:hashtag hashtag :n n}))

(defn hashtag-recent [hashtag n]
  (run-python "hashtag_recent" {:hashtag hashtag :n n}))

(defn post-info [url]
  (run-python "post_info" {:url url}))

(defn post-comment [url text]
  (run-python "post_comment" {:url url :text text}))

(defn user-posts [username n]
  (run-python "user_posts" {:username username :n n}))

(defn user-info [username]
  (run-python "user_info" {:username username}))

(defn search-hashtags [query]
  (run-python "search_hashtags" {:query query}))

(defn account-new-posts [username n]
  (run-python "account_new_posts" {:username username :n n}))

(defn mark-commented [shortcode]
  (run-python "mark_commented" {:shortcode shortcode}))

(defn generate-image [args]
  (run-python "generate_image" args))

(defn notifications [n]
  (run-python "notifications" {:n n}))

(defn follow-user [username]
  (run-python "follow_user" {:username username}))

(defn upload-photo [path caption]
  (run-python "upload_photo" {:path path :caption caption}))
