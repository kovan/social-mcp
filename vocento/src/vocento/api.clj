(ns vocento.api
  "Vocento comments API client."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [vocento.auth :as auth]))

(def ^:private comments-api "https://comments.vocento.com")

;; All Vocento regional publications
(def ^:private sites
  {"elcorreo.com"          {:domain "www.elcorreo.com"          :category "elcorreo"}
   "diariovasco.com"       {:domain "www.diariovasco.com"       :category "diariovasco"}
   "elnortedecastilla.es"  {:domain "www.elnortedecastilla.es"  :category "elnortedecastilla"}
   "lasprovincias.es"      {:domain "www.lasprovincias.es"      :category "lasprovincias"}
   "ideal.es"              {:domain "www.ideal.es"              :category "ideal"}
   "elcomercio.es"         {:domain "www.elcomercio.es"         :category "elcomercio"}
   "larioja.com"           {:domain "www.larioja.com"           :category "larioja"}
   "diariosur.es"          {:domain "www.diariosur.es"          :category "diariosur"}
   "hoy.es"                {:domain "www.hoy.es"                :category "hoy"}
   "laverdad.es"           {:domain "www.laverdad.es"           :category "laverdad"}
   "abc.es"                {:domain "www.abc.es"                :category "abc"}})

(defn- site-config
  "Derive Vocento site config from an article URL or site key (e.g. 'diariovasco.com')."
  [url-or-site]
  (let [key (if (str/starts-with? url-or-site "http")
              (-> (java.net.URI. url-or-site) .getHost
                  (str/replace #"^www\." ""))
              (str/replace url-or-site #"^www\." ""))]
    (or (get sites key)
        ;; fallback: derive from hostname
        {:domain (str "www." key)
         :category (first (str/split key #"\."))})))

(defn- fetch-json [url]
  (let [ua "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"
        pb (ProcessBuilder. ["curl" "-sSL" "-A" ua url])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- post-json [url body]
  (let [json-str (json/write-str body)
        pb (ProcessBuilder. ["curl" "-sS" "-X" "POST"
                              "-H" "Content-Type: application/json"
                              "-H" "Accept: application/json"
                              "-d" json-str
                              url])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- extract-article-id
  "Fetch article HTML and extract the streamId (data-comment-streamid)."
  [article-url]
  (let [ua "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"
        pb (ProcessBuilder. ["curl" "-sSL" "-A" ua article-url])
        proc (.start pb)
        html (slurp (.getInputStream proc))
        _ (.waitFor proc)
        m (or (re-find #"newsId:\s*'([a-f0-9-]+)'" html)
              (re-find #"newsId\"?:\s*\"([a-f0-9-]+)\"" html))]
    (when m (second m))))

(defn front-page
  "Get article URLs from the front page. site is a domain like 'diariovasco.com' (default: elcorreo.com)."
  [& {:keys [site] :or {site "elcorreo.com"}}]
  (let [ua "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"
        {:keys [domain]} (site-config site)
        site-url (str "https://" domain)
        pb (ProcessBuilder. ["curl" "-sSL" "-A" ua site-url])
        proc (.start pb)
        html (slurp (.getInputStream proc))
        _ (.waitFor proc)
        pattern (re-pattern (str "\"(https://" (java.util.regex.Pattern/quote domain) "/[a-z][^\"]+\\.html)\""))
        matcher (re-matcher pattern html)
        urls (loop [results []]
               (if (.find matcher)
                 (recur (conj results (.group matcher 1)))
                 results))]
    (->> urls
         (remove #(str/includes? % "de-tiendas"))
         (remove #(str/includes? % "#"))
         distinct
         vec)))

(defn list-comments
  "List comments for an article by streamId. article-url is used to derive the Vocento domain."
  [stream-id article-url & {:keys [page page-size sort]
                             :or {page 0 page-size 20 sort "dateDesc"}}]
  (let [{:keys [domain]} (site-config article-url)
        url (str comments-api "/api/v2/comment/list"
                 "?anonymous=true"
                 "&domain=" domain
                 "&streamId=" stream-id
                 "&page=" page
                 "&pageSize=" page-size
                 "&sort=" sort)]
    (fetch-json url)))

(defn post-comment
  "Post a comment to an article. article-url is used to derive the Vocento domain/category."
  [stream-id article-url text & {:keys [parent-id] :or {parent-id "root"}}]
  (let [user (auth/get-user)
        _ (when-not user (throw (ex-info "Not logged in to elcorreo.com in Chrome" {})))
        {:keys [domain category]} (site-config article-url)
        body {:streamId stream-id
              :categoryId category
              :domain domain
              :parentCommentId parent-id
              :markdown text
              :commenterId "web"
              :postId ""
              :user {:uid (:uid user)
                     :uidSignature (:uidSignature user)
                     :signatureTimestamp (:signatureTimestamp user)
                     :domain domain
                     :categoryId category
                     :email (:email user)
                     :name (:firstName user)
                     :lastName (:lastName user)
                     :userType (:userType user)
                     :nickName (:nickName user)
                     :tags ""
                     :photo (:photo user)
                     :link (:link user)}}]
    (post-json (str comments-api "/api/v2/comment/new") body)))

(defn trending
  "Get trending/most viewed articles. site is a domain like 'diariovasco.com' (default: elcorreo.com)."
  [& {:keys [site] :or {site "elcorreo.com"}}]
  (let [ua "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"
        {:keys [domain]} (site-config site)
        url (str "https://" domain "/top-noticias/mas-vistos.html")
        pb (ProcessBuilder. ["curl" "-sSL" "-A" ua url])
        proc (.start pb)
        html (slurp (.getInputStream proc))
        _ (.waitFor proc)
        escaped (java.util.regex.Pattern/quote domain)
        pattern (re-pattern (str "href=\"(https://" escaped "/[^\"]+\\.html)#vtm_modulosEngag=lomas:top-noticias:noticia:(\\d+)\""))
        matcher (re-matcher pattern html)
        results (loop [acc []]
                  (if (.find matcher)
                    (let [article-url (.group matcher 1)
                          rank (Integer/parseInt (.group matcher 2))
                          slug (-> article-url
                                   (str/replace (re-pattern (str "https://" escaped "/")) "")
                                   (str/replace #"-\d+-\w+\.html$" "")
                                   (str/replace #"/" " > ")
                                   (str/replace #"-" " "))]
                      (recur (conj acc {:url article-url :rank rank :title slug})))
                    acc))]
    (->> results
         (sort-by :rank)
         (map :url)
         distinct
         (map (fn [u] (first (filter #(= (:url %) u) results))))
         vec)))

(defn delete-comment
  "Delete a comment by its ID. article-url is used to derive the Vocento domain."
  [comment-id article-url]
  (let [user (auth/get-user)
        _ (when-not user (throw (ex-info "Not logged in to elcorreo.com in Chrome" {})))
        {:keys [domain category]} (site-config (or article-url "elcorreo.com"))
        body {:commentId comment-id
              :domain domain
              :categoryId category
              :user {:uid (:uid user)
                     :uidSignature (:uidSignature user)
                     :signatureTimestamp (:signatureTimestamp user)
                     :domain domain
                     :categoryId category
                     :email (:email user)
                     :name (:firstName user)
                     :lastName (:lastName user)
                     :userType (:userType user)
                     :nickName (:nickName user)
                     :tags ""
                     :photo (:photo user)
                     :link (:link user)}}]
    (post-json (str comments-api "/api/v2/comment/delete") body)))

(defn get-stream-id
  "Get the streamId for an article URL."
  [article-url]
  (extract-article-id article-url))
