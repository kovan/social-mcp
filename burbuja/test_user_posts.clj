;; Standalone test: fetch user posts via member profile + search/member
;; Usage: cd ~/social-mcp/burbuja && clojure -M test_user_posts.clj Nico

(require '[burbuja.cookies :as chrome]
         '[clojure.string :as str]
         '[clojure.java.io :as io])
(import '[org.jsoup Jsoup]
        '[org.jsoup.nodes Element]
        '[java.net URLEncoder])

(def ua "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(def cookie-file
  (let [cks (chrome/get-cookies "burbuja.info")
        f (java.io.File/createTempFile "burbuja-cookies" ".txt")]
    (.deleteOnExit f)
    (with-open [w (io/writer f)]
      (.write w "# Netscape HTTP Cookie File\n")
      (doseq [{:keys [name value path host]} cks]
        (when (and (seq value) (some? host))
          (.write w (str host "\t"
                     (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                     path "\tTRUE\t0\t" name "\t" value "\n"))))
      (when-not (some #(= (:name %) "xf_consent") cks)
        (.write w ".burbuja.info\tTRUE\t/\tFALSE\t0\txf_consent\t1\n")))
    (.getAbsolutePath f)))

(defn curl-get [url]
  (let [body-file (java.io.File/createTempFile "curl-body" ".html")
        args ["curl" "-sSL"
              "-b" cookie-file "-c" cookie-file
              "-H" (str "User-Agent: " ua)
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              url]
        pb (ProcessBuilder. ^java.util.List (vec args))
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        _ (slurp (.getErrorStream proc))
        _ (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    {:status (parse-long status-str) :body body}))

(defn resolve-user-id [username]
  (let [url (str "https://www.burbuja.info/inmobiliaria/members/?username=" (URLEncoder/encode username "UTF-8"))
        resp (curl-get url)]
    (when (= 200 (:status resp))
      (let [doc (Jsoup/parse ^String (:body resp))
            member-link (some->> (.select doc "a[href*=/members/]")
                                 (map #(.attr ^Element % "href"))
                                 (filter #(re-find #"\.\d+/" %))
                                 first)]
        (when member-link
          (second (re-find #"\.(\d+)/?" member-link)))))))

(defn parse-search-item [^Element item]
  (let [;; Thread title - first link with href containing /temas/
        title-link (or (.selectFirst item "a[href*=/temas/]")
                       (.selectFirst item "h3 a")
                       (.selectFirst item ".contentRow-title a"))
        title (when title-link (str/trim (.text title-link)))
        thread-url (when title-link
                     (let [href (.attr title-link "href")]
                       (if (str/starts-with? href "http")
                         href
                         (str "https://www.burbuja.info" href))))
        ;; Post snippet
        snippet-el (.selectFirst item ".contentRow-snippet")
        snippet (when snippet-el (str/trim (.text snippet-el)))
        ;; Meta info (date, forum, post number)
        minor-el (.selectFirst item ".contentRow-minor")
        meta-text (when minor-el (str/trim (.text minor-el)))
        ;; Time
        time-el (.selectFirst item "time")
        date (or (when time-el (.attr time-el "datetime"))
                 (when time-el (.text time-el)))
        ;; Post link
        post-link (.selectFirst item "a[href*=/posts/], a[href*=post-]")
        post-url (when post-link
                   (let [href (.attr post-link "href")]
                     (if (str/starts-with? href "http")
                       href
                       (str "https://www.burbuja.info" href))))]
    (when (or title snippet)
      {:title (or title "")
       :thread-url (or thread-url "")
       :snippet (or snippet "")
       :date (or date "")
       :meta (or meta-text "")
       :post-url (or post-url "")})))

(defn fetch-user-posts [username & {:keys [max-results] :or {max-results 20}}]
  (let [user-id (resolve-user-id username)]
    (when-not user-id
      (throw (ex-info (str "User not found: " username) {})))
    (let [url (str "https://www.burbuja.info/inmobiliaria/search/member?user_id=" user-id)
          resp (curl-get url)]
      (when-not (= 200 (:status resp))
        (throw (ex-info (str "HTTP " (:status resp) " fetching user posts") {})))
      (let [doc (Jsoup/parse ^String (:body resp))
            items (.select doc "li.block-row")
            posts (->> items
                       (map parse-search-item)
                       (filter some?)
                       (take max-results))]
        posts))))

(def username (or (first *command-line-args*) "chitta"))
(println "Fetching posts for:" username "\n")

(let [posts (fetch-user-posts username)]
  (println (str "Found " (count posts) " posts\n"))
  (doseq [{:keys [title snippet date meta post-url thread-url]} posts]
    (println (str "## " title))
    (when (seq date) (println (str "   Date: " date)))
    (when (seq snippet) (println (str "   " snippet)))
    (when (seq thread-url) (println (str "   Thread: " thread-url)))
    (when (seq post-url) (println (str "   Post: " post-url)))
    (when (seq meta) (println (str "   Meta: " meta)))
    (println)))

(println "Done.")
