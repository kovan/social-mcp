(ns hn.api
  "Read-only access to Hacker News via the Firebase API."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(def ^:private base "https://hacker-news.firebaseio.com/v0")

(defn- fetch-json [url]
  (let [pb (ProcessBuilder. ["curl" "-sSL" url])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn get-item [id]
  (fetch-json (str base "/item/" id ".json")))

(defn get-items
  "Fetch N items by ID in parallel."
  [ids n]
  (let [ids (take n ids)]
    (->> ids
         (pmap #(get-item %))
         (remove nil?)
         vec)))

(defn top-stories [] (fetch-json (str base "/topstories.json")))
(defn new-stories [] (fetch-json (str base "/newstories.json")))
(defn best-stories [] (fetch-json (str base "/beststories.json")))
(defn ask-stories [] (fetch-json (str base "/askstories.json")))
(defn show-stories [] (fetch-json (str base "/showstories.json")))
