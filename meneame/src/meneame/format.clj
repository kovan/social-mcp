(ns meneame.format
  "Format Menéame API responses into readable markdown."
  (:require [clojure.string :as str]))

(defn- relative-time [epoch-secs]
  (when epoch-secs
    (let [ts (cond (number? epoch-secs) (long epoch-secs)
                   (string? epoch-secs) (or (parse-long epoch-secs) 0)
                   :else 0)
          now (quot (System/currentTimeMillis) 1000)
          diff (- now ts)]
      (cond
        (< diff 60) "just now"
        (< diff 3600) (str (quot diff 60) "m ago")
        (< diff 86400) (str (quot diff 3600) "h ago")
        :else (str (quot diff 86400) "d ago")))))

(defn format-story [{:keys [id title user votes negatives karma comments
                             clicks url sub status sent_date content tags]}]
  (str votes " meneos"
       (when (and negatives (pos? negatives)) (str " | -" negatives))
       " | " (or user "?")
       " | " (or comments 0) " comments"
       (when clicks (str " | " clicks " clicks"))
       (when sub (str " | " sub))
       (when sent_date (str " | " (relative-time sent_date)))
       "\n" title
       (when (seq url) (str "\n" url))
       "\nhttps://www.meneame.net/story/" id
       (when (seq content)
         (str "\n" (subs content 0 (min 300 (count content)))
              (when (> (count content) 300) "...")))
       " [" id "]"))

(defn format-stories [data title]
  (let [stories (:objects data)]
    (if (seq stories)
      (str "# " title " (" (count stories) " stories)\n\n"
           (str/join "\n\n---\n\n"
             (map-indexed
               (fn [i story]
                 (str (inc i) ". " (format-story story)))
               stories)))
      (str "# " title "\n\nNo stories found."))))

(defn- strip-html [s]
  (when s
    (-> s
        (str/replace #"<br\s*/?>" "\n")
        (str/replace #"<p>" "\n")
        (str/replace #"<[^>]+>" "")
        (str/replace #"&amp;" "&")
        (str/replace #"&lt;" "<")
        (str/replace #"&gt;" ">")
        (str/replace #"&quot;" "\"")
        (str/replace #"&#39;" "'")
        str/trim)))

(defn format-comments [data story-title]
  (let [comments (:objects data)]
    (if (seq comments)
      (str "# Comments on: " (or story-title "story") " (" (count comments) ")\n\n"
           (str/join "\n\n"
             (map-indexed
               (fn [i {:keys [id user content votes karma date order]}]
                 (let [indent (if (and order (> order 0)) "  " "")]
                   (str indent "### " (or user "[deleted]")
                        " [" (or votes 0) " votes]"
                        (when date (str " " (relative-time date)))
                        " [comment:" id "]\n"
                        indent (strip-html content))))
               comments)))
      "No comments yet.")))
