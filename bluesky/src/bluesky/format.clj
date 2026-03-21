(ns bluesky.format
  (:require [clojure.string :as str]))

(defn- relative-time [iso-str]
  (when (seq iso-str)
    (try
      (let [inst (java.time.Instant/parse iso-str)
            now (java.time.Instant/now)
            diff (- (.getEpochSecond now) (.getEpochSecond inst))]
        (cond
          (< diff 60) "just now"
          (< diff 3600) (str (quot diff 60) "m ago")
          (< diff 86400) (str (quot diff 3600) "h ago")
          :else (str (quot diff 86400) "d ago")))
      (catch Exception _ nil))))

(defn- format-post
  "Format a single post map (the :post value from feed items)."
  [{:keys [uri author record likeCount repostCount replyCount]}]
  (let [{:keys [handle displayName]} author
        {:keys [text createdAt]} record
        time-str (relative-time createdAt)]
    (str "**" (or displayName handle) "** (@" handle ")"
         (when time-str (str " - " time-str)) "\n"
         text "\n"
         "Likes: " (or likeCount 0)
         " | Reposts: " (or repostCount 0)
         " | Replies: " (or replyCount 0) "\n"
         "URI: " uri)))

(defn format-feed [data title]
  (let [feed (:feed data)]
    (if (seq feed)
      (str "# " title " (" (count feed) ")\n\n"
           (str/join "\n---\n\n"
             (map-indexed
               (fn [i {:keys [post reason]}]
                 (str (inc i) ". "
                      (when reason "[Repost] ")
                      (format-post post)))
               feed)))
      (str "# " title "\n\nNo posts."))))

(defn- format-thread-node [node depth]
  (when node
    (let [{:keys [post replies notFound blocked]} node]
      (cond
        notFound (str (str/join "" (repeat depth "  ")) "[post not found]\n")
        blocked (str (str/join "" (repeat depth "  ")) "[blocked]\n")
        :else
        (let [indent (str/join "" (repeat depth "  "))
              lines (str/split-lines (format-post post))
              indented (str/join "\n" (map #(str indent %) lines))]
          (str indented "\n"
               (when (seq replies)
                 (str/join "" (map #(format-thread-node % (inc depth)) replies)))))))))

(defn format-thread [data]
  (let [thread (:thread data)]
    (if thread
      (str "# Thread\n\n" (format-thread-node thread 0))
      "Thread not found.")))

(defn format-notifications [data]
  (let [notifs (:notifications data)]
    (if (seq notifs)
      (str "# Notifications (" (count notifs) ")\n\n"
           (str/join "\n---\n\n"
             (map-indexed
               (fn [i {:keys [author reason reasonSubject record indexedAt isRead uri]}]
                 (let [{:keys [handle displayName]} author
                       time-str (relative-time indexedAt)]
                   (str (inc i) ". "
                        (when-not isRead "**[NEW]** ")
                        "**" (or displayName handle) "** (@" handle ")"
                        " - " reason
                        (when time-str (str " (" time-str ")")) "\n"
                        (when-let [text (:text record)] (str text "\n"))
                        (when (seq reasonSubject) (str "Post: " reasonSubject)))))
               notifs)))
      "# Notifications\n\nNone.")))
