(ns pikabu.format
  "Format Pikabu data into readable markdown."
  (:require [clojure.string :as str]))

(defn- relative-time [timestamp-str]
  (when (seq timestamp-str)
    (try
      (let [ts (Long/parseLong timestamp-str)
            now (quot (System/currentTimeMillis) 1000)
            diff (- now ts)]
        (cond
          (< diff 60) "just now"
          (< diff 3600) (str (quot diff 60) "m ago")
          (< diff 86400) (str (quot diff 3600) "h ago")
          :else (str (quot diff 86400) "d ago")))
      (catch Exception _ nil))))

(defn format-story
  "Format a single story for listing display."
  [i {:keys [id title author rating comments timestamp url community tags text]}]
  (str (inc i) ". "
       rating " pts | "
       author " | "
       comments " comments | "
       (or (relative-time timestamp) "")
       (when community (str " | " community))
       "\n" title
       "\n" url
       (when (seq tags) (str "\n[" (str/join ", " tags) "]"))
       (when (seq text)
         (str "\n" (subs text 0 (min 300 (count text)))
              (when (> (count text) 300) "...")))
       "\n[story:" id "]"))

(defn format-listing
  "Format a list of stories into markdown."
  [stories title]
  (if (seq stories)
    (str "# " title " (" (count stories) " stories)\n\n"
         (str/join "\n\n" (map-indexed format-story stories)))
    (str "# " title "\n\nNo stories found.")))

(defn- build-comment-tree
  "Build a tree of comments from flat list. Returns top-level comments with :children."
  [comments-list]
  (let [by-id (into {} (map (juxt :id identity) comments-list))
        children-map (group-by :parent comments-list)
        top-level (get children-map "0")]
    (letfn [(attach-children [comment depth]
              (let [kids (get children-map (:id comment))]
                (assoc comment
                  :depth depth
                  :children (when (and (seq kids) (< depth 4))
                              (mapv #(attach-children % (inc depth)) kids)))))]
      (mapv #(attach-children % 0) top-level))))

(defn- format-comment-node
  "Format a single comment and its children."
  [{:keys [author body rating date depth children]}]
  (let [indent (apply str (repeat (or depth 0) "  "))]
    (into
      [(str indent "### " (or author "[deleted]")
            " [" (or rating "0") " pts] "
            (or date "")
            "\n" indent (str/replace (or body "") "\n" (str "\n" indent)))]
      (when (seq children)
        (mapcat format-comment-node children)))))

(defn format-story-full
  "Format a full story with comments."
  [story comments-list]
  (let [{:keys [title author rating comments url community tags text]} story
        tree (when (seq comments-list)
               (build-comment-tree comments-list))]
    (str "# " title "\n"
         rating " pts | " author " | "
         comments " comments"
         (when community (str " | " community))
         "\n" url
         (when (seq tags) (str "\n[" (str/join ", " tags) "]"))
         (when (seq text) (str "\n\n" text))
         "\n\n---\n\n"
         (if (seq tree)
           (let [formatted (->> tree
                                (mapcat format-comment-node)
                                (remove nil?)
                                vec)]
             (str (count comments-list) " comments loaded\n\n"
                  (str/join "\n\n" formatted)))
           "No comments loaded."))))
