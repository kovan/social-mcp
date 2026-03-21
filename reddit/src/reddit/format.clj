(ns reddit.format
  "Format Reddit API data into readable markdown."
  (:require [clojure.string :as str]))

(defn- relative-time [epoch-secs]
  (when epoch-secs
    (let [now (quot (System/currentTimeMillis) 1000)
          diff (- now epoch-secs)]
      (cond
        (< diff 60) "just now"
        (< diff 3600) (str (quot diff 60) "m ago")
        (< diff 86400) (str (quot diff 3600) "h ago")
        :else (str (quot diff 86400) "d ago")))))

(defn format-post
  "Format a single post from a listing."
  [{:keys [data]}]
  (let [{:keys [title subreddit score num_comments author
                created_utc selftext url permalink id name
                link_flair_text]} data]
    (str (when score (str score " pts | "))
         "r/" subreddit " | "
         (or author "[deleted]") " | "
         (when num_comments (str num_comments " comments | "))
         (relative-time created_utc)
         (when (seq link_flair_text) (str " [" link_flair_text "]"))
         "\n" title
         "\nhttps://www.reddit.com" permalink
         (when (and (seq url)
                    (not (str/includes? url "reddit.com/r/")))
           (str "\n" url))
         (when (seq selftext)
           (str "\n" (subs selftext 0 (min 500 (count selftext)))
                (when (> (count selftext) 500) "...")))
         "\n[" name "]")))

(defn format-listing
  "Format a listing response into markdown."
  [response title]
  (let [posts (get-in response [:data :children])]
    (if (seq posts)
      (str "# " title " (" (count posts) " posts)\n\n"
           (str/join "\n\n"
             (map-indexed
               (fn [i post]
                 (str (inc i) ". " (format-post post)))
               posts)))
      (str "# " title "\n\nNo posts found."))))

(defn- format-comment-node
  "Format a single comment and its children recursively."
  [{:keys [data kind]} depth max-depth]
  (when (and (= kind "t1") data (not (:body data)))
    nil)
  (when (and (= kind "t1") data)
    (let [{:keys [author body score created_utc name children]} data
          indent (apply str (repeat depth "  "))
          replies (when (and (map? (:replies data))
                             (< depth max-depth))
                    (get-in data [:replies :data :children]))]
      (into
        [(str indent "### " (or author "[deleted]")
              " [" score " pts] "
              (relative-time created_utc)
              " [" name "]\n"
              (when (seq body)
                (str indent
                     (str/replace body "\n" (str "\n" indent)))))]
        (when (seq replies)
          (mapcat #(format-comment-node % (inc depth) max-depth)
                  (remove #(= (:kind %) "more") replies)))))))

(defn format-thread
  "Format a thread response (post + comments) into markdown."
  [data]
  (when-not (sequential? data)
    (throw (ex-info "Reddit API returned unexpected response - check subreddit and post_id parameters"
                    {:response (str (pr-str data))})))
  (let [[post-listing comments-listing] data
        post (get-in post-listing [:data :children 0 :data])
        comments (get-in comments-listing [:data :children])
        header (str "# " (:title post) "\n"
                    (when (:score post) (str (:score post) " pts | "))
                    "r/" (:subreddit post) " | "
                    (or (:author post) "[deleted]") " | "
                    (when (:num_comments post) (str (:num_comments post) " comments | "))
                    (relative-time (:created_utc post))
                    "\nhttps://www.reddit.com" (:permalink post)
                    " [" (:name post) "]"
                    (when (seq (:selftext post))
                      (str "\n\n" (:selftext post)))
                    "\n\n---\n\n")]
    (str header
         (if (seq comments)
           (let [formatted (->> comments
                                (mapcat #(format-comment-node % 0 4))
                                (remove nil?)
                                vec)]
             (str (count formatted) " comments loaded\n\n"
                  (str/join "\n\n" formatted)))
           "No comments yet."))))
