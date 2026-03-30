(ns pikabu.api
  "Read-only access to Pikabu via HTML scraping and XML comments endpoint."
  (:require [clojure.string :as str]
            [clojure.data.xml :as xml])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]
           [org.jsoup.select Elements]
           [java.net URLEncoder]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(defn- fetch-html
  "Fetch a URL and return a Jsoup Document."
  ^Document [^String url]
  (-> (Jsoup/connect url)
      (.userAgent ua)
      (.timeout 15000)
      (.get)))

(defn- fetch-bytes
  "Fetch raw bytes from URL via curl (for windows-1251 encoded pages)."
  [^String url]
  (let [pb (ProcessBuilder. ["curl" "-sSL" "-H" (str "User-Agent: " ua) url])
        proc (.start pb)
        out (.readAllBytes (.getInputStream proc))
        _ (.waitFor proc)]
    out))

(defn- parse-html
  "Parse HTML bytes with specified charset."
  ^Document [^bytes html-bytes ^String charset ^String base-uri]
  (Jsoup/parse (java.io.ByteArrayInputStream. html-bytes) charset base-uri))

(defn- extract-story
  "Extract story data from an <article> element."
  [^Element article]
  (let [story-id (.attr article "data-story-id")
        rating (.attr article "data-rating")
        author (.attr article "data-author-name")
        comments (.attr article "data-comments")
        timestamp (.attr article "data-timestamp")
        title-el (.selectFirst article ".story__title-link")
        title (when title-el (.text title-el))
        href (when title-el (.attr title-el "href"))
        community-el (.selectFirst article ".story__community-name")
        community (when community-el (.text community-el))
        tags-els (.select article ".tags__tag")
        tags (mapv #(.attr ^Element % "data-tag") tags-els)
        text-blocks (.select article ".story-block_type_text")
        text (str/join "\n" (map #(.text ^Element %) text-blocks))]
    (when (seq story-id)
      {:id story-id
       :title (or title "")
       :author (or author "")
       :rating (or rating "0")
       :comments (or comments "0")
       :timestamp (or timestamp "")
       :url (or href (str "https://pikabu.ru/story/_" story-id))
       :community community
       :tags (when (seq tags) tags)
       :text (when (seq text) text)})))

(defn listing
  "Fetch stories from a Pikabu listing page.
   path is one of: nil (hot), \"new\", \"best\", or \"community/NAME\"."
  [path n & {:keys [page]}]
  (let [base "https://pikabu.ru"
        url-path (cond
                   (nil? path) ""
                   (str/starts-with? path "community/") (str "/" path)
                   :else (str "/" path))
        page-param (when (and page (> page 1)) (str "?page=" page))
        url (str base url-path page-param)
        html-bytes (fetch-bytes url)
        doc (parse-html html-bytes "windows-1251" url)
        articles (.select doc "article.story[data-story-id]")
        stories (->> articles
                     (map extract-story)
                     (remove nil?)
                     (take n)
                     vec)]
    stories))

(defn story
  "Fetch a single story page by ID. Returns story data with full text."
  [story-id]
  (let [url (str "https://pikabu.ru/story/_" story-id)
        html-bytes (fetch-bytes url)
        doc (parse-html html-bytes "windows-1251" url)
        article (.selectFirst doc "article.story[data-story-id]")]
    (when article
      (extract-story article))))

(defn comments
  "Fetch comments for a story via the XML endpoint."
  [story-id]
  (let [url (str "https://pikabu.ru/generate_xml_comm.php?id=" story-id)
        html-bytes (fetch-bytes url)
        xml-str (String. html-bytes "UTF-8")
        parsed (xml/parse-str xml-str)]
    (->> (:content parsed)
         (filter #(= (:tag %) :comment))
         (mapv (fn [node]
                 (let [attrs (:attrs node)
                       body (apply str (map #(if (string? %) % (or (first (:content %)) ""))
                                            (:content node)))]
                   {:id (:id attrs)
                    :rating (:rating attrs)
                    :author (:nick attrs)
                    :parent (:answer attrs)
                    :date (:date attrs)
                    :body (-> body
                              (str/replace #"<[^>]+>" "")
                              str/trim)}))))))

(defn search
  "Search Pikabu stories."
  [query n & {:keys [page]}]
  (let [encoded (URLEncoder/encode query "UTF-8")
        pg (or page 1)
        url (str "https://pikabu.ru/search?q=" encoded "&page=" pg)
        html-bytes (fetch-bytes url)
        doc (parse-html html-bytes "windows-1251" url)
        articles (.select doc "article.story[data-story-id]")
        stories (->> articles
                     (map extract-story)
                     (remove nil?)
                     (take n)
                     vec)]
    stories))
