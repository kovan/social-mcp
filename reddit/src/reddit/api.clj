(ns reddit.api
  "Read-only access to Reddit via public JSON endpoints."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(defn- fetch-json [url]
  (let [pb (ProcessBuilder. ["curl" "-sSL"
                              "-H" (str "User-Agent: " ua)
                              url])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn listing
  "Fetch a listing from Reddit. sort is one of hot/new/top/rising.
   subreddit is nil for front page."
  [subreddit sort-by n & {:keys [time-period]}]
  (let [sub-path (if subreddit (str "/r/" subreddit) "")
        params (cond-> (str "?limit=" n "&raw_json=1")
                 time-period (str "&t=" time-period))
        url (str "https://www.reddit.com" sub-path "/" (name sort-by) ".json" params)]
    (fetch-json url)))

(defn thread
  "Fetch a thread and its comments. Returns [post-data comments-data]."
  [subreddit post-id & {:keys [sort limit]}]
  (let [params (cond-> "?raw_json=1"
                 sort (str "&sort=" sort)
                 limit (str "&limit=" limit))
        url (str "https://www.reddit.com/r/" subreddit "/comments/" post-id ".json" params)]
    (fetch-json url)))

(defn search
  "Search Reddit. Returns listing."
  [query n & {:keys [subreddit sort time-period]}]
  (let [sub-path (if subreddit (str "/r/" subreddit) "")
        sort-param (or sort "relevance")
        time-param (or time-period "all")
        url (str "https://www.reddit.com" sub-path "/search.json"
                 "?q=" (java.net.URLEncoder/encode query "UTF-8")
                 "&sort=" sort-param
                 "&t=" time-param
                 "&limit=" n
                 "&raw_json=1")]
    (fetch-json url)))

(defn user-posts
  "Fetch a user's recent posts or comments."
  [username type-str n]
  (let [url (str "https://www.reddit.com/user/" username "/" type-str ".json"
                 "?limit=" n "&raw_json=1")]
    (fetch-json url)))
