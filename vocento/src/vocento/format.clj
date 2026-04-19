(ns vocento.format
  "Format Vocento API responses into readable text."
  (:require [clojure.string :as str]))

(defn format-front-page [urls]
  (if (empty? urls)
    "No articles found."
    (->> urls
         (map-indexed (fn [i url]
                        (let [;; extract title-like slug from URL
                              slug (-> url
                                       (str/replace #"https://www\.[^/]+/" "")
                                       (str/replace #"-\d+-\w+\.html$" "")
                                       (str/replace #"/" " > ")
                                       (str/replace #"-" " "))]
                          (str (inc i) ". " slug "\n   " url))))
         (str/join "\n\n"))))

(defn format-comments [data]
  (let [comments (:comments data)
        commenters (:commenters data)
        attrs (:attributes data)
        title (:title attrs)
        count (:commentCount attrs)]
    (if (empty? comments)
      (str title "\nNo comments yet.")
      (str title " (" count " comments)\n\n"
           (->> comments
                (map (fn [c]
                       (let [cid (:commenterId c)
                             commenter (get commenters (keyword cid))
                             nick (or (:nickName commenter) "anonymous")
                             text (-> (:html c)
                                      (str/replace #"<[^>]+>" "")
                                      str/trim)
                             parent (:parentCommentId c)
                             prefix (if (= parent "root") "" "  > ")
                             score (:score c)
                             id (:commentId c)]
                         (str prefix nick " [+" (:posVotes c) "/-" (:negVotes c) "]"
                              "\n" prefix text
                              "\n" prefix "[id:" (subs id 0 12) "]"))))
                (str/join "\n\n"))))))

(defn format-trending [articles]
  (if (empty? articles)
    "No trending articles found."
    (->> articles
         (map-indexed (fn [i {:keys [url title]}]
                        (str (inc i) ". " title "\n   " url)))
         (str/join "\n\n"))))

(defn format-post-result [result]
  (if (:success result)
    (str "Comment posted successfully. State: " (:state result))
    (str "Failed to post comment: " (:message result))))
