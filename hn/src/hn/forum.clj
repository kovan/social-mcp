(ns hn.forum
  "High-level HN operations combining API reads and web interactions."
  (:require [clojure.string :as str]
            [hn.api :as api]
            [hn.web :as web]))

(defn- format-story [{:keys [id title url score by descendants time text]}]
  (let [ts (when time
             (str (java.time.Instant/ofEpochSecond time)))]
    (str (when score (str score " points | "))
         (when by (str by " | "))
         (when descendants (str descendants " comments | "))
         (when ts ts) "\n"
         title
         (when (seq url) (str "\n" url))
         (when (seq text) (str "\n" text))
         "\nhttps://news.ycombinator.com/item?id=" id)))

(defn list-stories
  "List stories from a feed. feed is one of :top :new :best :ask :show"
  [feed n]
  (let [ids (case feed
              :top (api/top-stories)
              :new (api/new-stories)
              :best (api/best-stories)
              :ask (api/ask-stories)
              :show (api/show-stories))
        items (api/get-items ids n)]
    (str "# " (str/capitalize (name feed)) " Stories (" (count items) ")\n\n"
         (str/join "\n\n"
           (map-indexed
             (fn [i item]
               (str (inc i) ". " (format-story item)))
             items)))))

(defn- format-comment [{:keys [id by text time kids]} depth]
  (let [indent (apply str (repeat depth "  "))
        ts (when time (str (java.time.Instant/ofEpochSecond time)))]
    (str indent "### " (or by "[deleted]")
         (when ts (str " (" ts ")"))
         " [item:" id "]\n"
         (when (seq text)
           (str indent (str/replace text "\n" (str "\n" indent)))))))

(defn- build-comment-tree
  "Recursively fetch and format comments up to max-depth."
  [item-id depth max-depth]
  (when (< depth max-depth)
    (let [item (api/get-item item-id)]
      (when (and item (not (:deleted item)) (not (:dead item)))
        (let [self (format-comment item depth)
              children (when (:kids item)
                         (->> (:kids item)
                              (mapcat #(build-comment-tree % (inc depth) max-depth))
                              vec))]
          (into [self] children))))))

(defn read-thread
  "Read a story and its comments."
  [item-id]
  (let [story (api/get-item item-id)]
    (when-not story
      (throw (ex-info (str "Item " item-id " not found") {})))
    (let [comments (when (:kids story)
                     (->> (:kids story)
                          (mapcat #(build-comment-tree % 0 4))
                          (remove nil?)
                          vec))
          header (str "# " (or (:title story) "Comment thread") "\n"
                      (format-story story) "\n"
                      (when (seq (:text story)) (str "\n" (:text story) "\n"))
                      "\n---\n\n")]
      (str header
           (if (seq comments)
             (str (count comments) " comments loaded\n\n"
                  (str/join "\n\n" comments))
             "No comments yet.")))))

(defn reply-comment [item-id message]
  (web/reply-comment item-id message))

(defn submit-story [title & {:keys [url text]}]
  (web/submit-story title :url url :text text))

(defn list-replies []
  (web/list-replies))
