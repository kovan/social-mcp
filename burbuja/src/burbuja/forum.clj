(ns burbuja.forum
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [burbuja.cookies :as chrome])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]
           [java.net URLEncoder]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

;; Cookie jar file shared across requests
(def ^:private cookie-jar (atom nil))

(defn- init-cookies! []
  (let [cks (chrome/get-cookies "burbuja.info")
        f (java.io.File/createTempFile "burbuja-cookies" ".txt")]
    (.deleteOnExit f)
    (with-open [w (io/writer f)]
      (.write w "# Netscape HTTP Cookie File\n")
      (doseq [{:keys [name value path host]} cks]
        (when (and (seq value) (some? host))
          (.write w (str host "\t"
                         (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                         path "\t"
                         "TRUE\t"
                         "0\t"
                         name "\t"
                         value "\n"))))
      ;; xf_consent is stored in LocalStorage by burbuja.info, not the cookie jar.
      ;; Inject it manually so XenForo allows POST actions.
      (when-not (some #(= (:name %) "xf_consent") cks)
        (.write w ".burbuja.info\tTRUE\t/\tFALSE\t0\txf_consent\t1\n")))
    (reset! cookie-jar (.getAbsolutePath f))
    (binding [*out* *err*]
      (println (str "Loaded " (count cks) " cookies for burbuja.info")))))

(defn- curl-request [url & {:keys [method params ajax?]}]
  (when (nil? @cookie-jar) (init-cookies!))
  (let [body-file (java.io.File/createTempFile "curl-body" ".html")
        args (cond-> ["curl" "-sSL"
                      "-b" @cookie-jar "-c" @cookie-jar
                      "-H" (str "User-Agent: " ua)
                      "-o" (.getAbsolutePath body-file)
                      "-w" "%{http_code}"]
               (= method :post)
               (into ["-X" "POST"])

               (seq params)
               (into ["-d" (str/join "&"
                             (map (fn [[k v]]
                                    (str (URLEncoder/encode k "UTF-8") "="
                                         (URLEncoder/encode v "UTF-8")))
                                  params))])

               ajax?
               (into ["-H" "X-Requested-With: XMLHttpRequest"])

               true
               (conj url))
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "curl error:" err)))
    {:status (or (parse-long status-str) 0) :body body}))

(defn- get-page [url]
  (curl-request url))

(defn- post-form [url params & {:keys [ajax?]}]
  (curl-request url :method :post :params params :ajax? ajax?))

(defn- extract-csrf [body]
  (let [doc (Jsoup/parse ^String body)]
    (or (some-> (.selectFirst doc "input[name=_xfToken]") (.val))
        (some-> (.selectFirst doc "html[data-csrf]") (.attr "data-csrf")))))

;; Sentinel for preserving newlines through Jsoup text extraction
(def ^:private nl "\u2424")

(defn- extract-content
  "Extract readable text from a .bbWrapper element."
  [^Element wrapper]
  (when wrapper
    (-> (.html wrapper)
        (str/replace #"(?i)<br\s*/?>" "\n")
        (str/replace #"(?i)</(?:p|div|li)>" "\n")
        (str/replace #"<[^>]+>" "")
        (str/replace "\n" nl)
        Jsoup/parse
        (.text)
        (str/replace nl "\n")
        (str/replace #"\n{3,}" "\n\n")
        str/trim)))

(defn- parse-post [^Element article]
  (let [author (.attr article "data-author")
        post-id (str/replace (.attr article "data-content") "post-" "")
        date (or (some-> (.selectFirst article "time[datetime]") (.attr "datetime")) "")
        number (or (some-> (.selectFirst article ".message-attribution-opposite a:last-child") (.text)) "")
        wrapper (.selectFirst article ".bbWrapper")
        content (extract-content wrapper)]
    {:author author
     :post-id post-id
     :date date
     :number number
     :content (or content "")}))

(defn- format-post [{:keys [author post-id date number content]}]
  (str "### " (when (seq number) (str "#" number " - "))
       author
       (when (seq date) (str " (" date ")"))
       " [post:" post-id "]\n\n"
       content))

(defn list-alerts
  ([] (list-alerts 3 nil))
  ([num-pages max-hours]
   (let [base "https://www.burbuja.info/inmobiliaria/account/alerts"
         now-ms (System/currentTimeMillis)
         docs (for [p (range 1 (inc num-pages))]
                (let [url (if (= p 1) base (str base "?page=" p))
                      resp (get-page url)]
                  (when (= 200 (:status resp))
                    (Jsoup/parse ^String (:body resp)))))
         alerts (for [doc docs
                      :when doc
                      li (.select doc "li[data-alert-id]")
                      :let [body (.text li)
                            is-quote (str/includes? body "citó tu mensaje")
                            post-link (some-> (.selectFirst li "a.fauxBlockLink-blockLink")
                                              (.attr "href"))
                            user (some-> (.selectFirst li "a.username") (.text))
                            time-el (some-> (.selectFirst li "time") (.attr "data-short"))
                            epoch-str (some-> (.selectFirst li "time") (.attr "data-time"))
                            epoch-ms (when (seq epoch-str)
                                       (try (* (parse-long epoch-str) 1000)
                                            (catch Exception _ nil)))
                            age-hours (when epoch-ms
                                        (/ (double (- now-ms epoch-ms)) 3600000.0))]
                      :when (and is-quote (seq post-link))
                      :when (or (nil? max-hours)
                                (nil? age-hours)
                                (<= age-hours max-hours))]
                  {:user (or user "?")
                   :time (or time-el "?")
                   :url (if (str/starts-with? post-link "http")
                          post-link
                          (str "https://www.burbuja.info" post-link))})]
     (str "# Alerts (" (count alerts) " quotes)\n\n"
          (str/join "\n" (map (fn [{:keys [user time url]}]
                                (str "[" time "] " user ": " url))
                              alerts))))))

(defn list-new-posts []
  (let [resp (get-page "https://www.burbuja.info/inmobiliaria/whats-new/posts/")]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching new posts") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          links (.select doc "a[data-preview-url]")
          seen (atom #{})
          threads (for [^Element a links
                        :let [href (.attr a "href")
                              title (str/trim (.text a))
                              clean (str/replace href #"(page-\d+|post-\d+|unread|/latest)/?$" "")]
                        :when (and (re-find #"/temas/" href)
                                   (seq title)
                                   (not (contains? @seen clean)))]
                    (do (swap! seen conj clean)
                        {:title title
                         :url (if (str/starts-with? clean "http")
                                clean
                                (str "https://www.burbuja.info" clean))}))]
      (str "# New posts (" (count threads) " threads)\n\n"
           (str/join "\n" (map-indexed
                            (fn [i {:keys [title url]}]
                              (str (inc i) ". " title "\n   " url))
                            threads))))))

(defn list-trending []
  (let [resp (get-page "https://www.burbuja.info/inmobiliaria/trending/")]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching trending") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          items (.select doc "a[data-preview-url]")
          threads (for [^Element a items
                        :let [href (.attr a "href")
                              title (str/trim (.text a))]
                        :when (and (re-find #"/temas/" href) (seq title))]
                    {:title title
                     :url (if (str/starts-with? href "http")
                            href
                            (str "https://www.burbuja.info" href))})]
      (str "# Trending threads (" (count threads) ")\n\n"
           (str/join "\n" (map-indexed
                            (fn [i {:keys [title url]}]
                              (str (inc i) ". " title "\n   " url))
                            threads))))))

(defn read-thread [thread-url]
  (let [resp (get-page thread-url)
        _ (when-not (= 200 (:status resp))
            (throw (ex-info (str "HTTP " (:status resp) " fetching thread") {:url thread-url})))
        doc0 (Jsoup/parse ^String (:body resp))
        ;; If the page has no posts (e.g. post-level URL served a redirect page),
        ;; resolve to the thread page via the canonical link.
        doc (if (and (empty? (.select doc0 "article.message--post"))
                     (some-> (.selectFirst doc0 "link[rel=canonical]") (.attr "href")
                             (re-find #"/temas/")))
              (let [canonical (-> (.selectFirst doc0 "link[rel=canonical]") (.attr "href")
                                  (str/replace #"#.*$" ""))
                    r (get-page canonical)
                    _ (when-not (= 200 (:status r))
                        (throw (ex-info (str "HTTP " (:status r) " fetching thread") {:url canonical})))]
                (Jsoup/parse ^String (:body r)))
              doc0)
        title (or (some-> (.selectFirst doc "h1.p-title-value") (.text)) "Untitled")
        articles (.select doc "article.message--post")
        posts (mapv parse-post articles)
        last-page-el (.selectFirst doc ".pageNav-page:last-child a")
        total-pages (or (some-> last-page-el (.text) parse-long) 1)
        current-page-el (.selectFirst doc ".pageNav-page--current a, .pageNav-page--current span")
        current-page (or (some-> current-page-el (.text) parse-long) 1)]
    (str "# " title "\n"
         "Page " current-page " of " total-pages
         " (" (count posts) " posts on this page)\n\n"
         (str/join "\n\n---\n\n" (map format-post posts)))))

(defn delete-post [post-url]
  (let [resp (get-page post-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching post") {:url post-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          post-id (or (second (re-find #"post-(\d+)" post-url))
                      (second (re-find #"posts/(\d+)" post-url))
                      (throw (ex-info "Cannot extract post ID from URL" {:url post-url})))
          csrf (or (extract-csrf (:body resp))
                   (throw (ex-info "No CSRF token on page" {})))
          delete-url (str "https://www.burbuja.info/inmobiliaria/posts/" post-id "/delete")
          del-resp (post-form delete-url
                    {"_xfToken" csrf
                     "_xfRequestUri" (or (some-> (.selectFirst doc "link[rel=canonical]") (.attr "href"))
                                         post-url)
                     "_xfNoRedirect" "1"
                     "_xfResponseType" "json"}
                    :ajax? true)]
      (if (<= 200 (:status del-resp) 303)
        (str "Post " post-id " deleted successfully.")
        (throw (ex-info (str "Delete failed: HTTP " (:status del-resp))
                        {:body (subs (or (:body del-resp) "")
                                     0 (min 500 (count (or (:body del-resp) ""))))}))))))

(defn- resolve-user-id
  "Look up a burbuja.info user ID from their username."
  [username]
  (let [url (str "https://www.burbuja.info/inmobiliaria/members/?username="
                 (URLEncoder/encode username "UTF-8"))
        resp (get-page url)]
    (when (= 200 (:status resp))
      (let [doc (Jsoup/parse ^String (:body resp))
            member-link (some->> (.select doc "a[href*=/members/]")
                                 (map #(.attr ^Element % "href"))
                                 (filter #(re-find #"\.\d+/" %))
                                 first)]
        (when member-link
          (second (re-find #"\.(\d+)/?" member-link)))))))

(defn- parse-search-item [^Element item]
  (let [title-link (or (.selectFirst item "a[href*=/temas/]")
                       (.selectFirst item "h3 a")
                       (.selectFirst item ".contentRow-title a"))
        title (when title-link (str/trim (.text title-link)))
        thread-url (when title-link
                     (let [href (.attr title-link "href")]
                       (if (str/starts-with? href "http")
                         href
                         (str "https://www.burbuja.info" href))))
        snippet-el (.selectFirst item ".contentRow-snippet")
        snippet (when snippet-el (str/trim (.text snippet-el)))
        minor-el (.selectFirst item ".contentRow-minor")
        meta-text (when minor-el (str/trim (.text minor-el)))
        time-el (.selectFirst item "time")
        date (when time-el
               (or (not-empty (.attr time-el "datetime"))
                   (.text time-el)))]
    (when (or title snippet)
      {:title (or title "")
       :url (or thread-url "")
       :snippet (or snippet "")
       :date (or date "")
       :meta (or meta-text "")})))

(defn user-posts
  "Fetch recent posts by a specific username via their member profile."
  [username & {:keys [max-results] :or {max-results 20}}]
  (let [user-id (resolve-user-id username)]
    (when-not user-id
      (throw (ex-info (str "User not found: " username) {})))
    (let [url (str "https://www.burbuja.info/inmobiliaria/search/member?user_id=" user-id)
          resp (get-page url)]
      (when-not (= 200 (:status resp))
        (throw (ex-info (str "HTTP " (:status resp) " fetching user posts") {})))
      (let [doc (Jsoup/parse ^String (:body resp))
            items (.select doc "li.block-row")
            posts (->> items
                       (map parse-search-item)
                       (filter some?)
                       (take max-results))]
        (if (seq posts)
          (str "# Posts by " username " (" (count posts) " results)\n\n"
               (str/join "\n\n---\n\n"
                 (map-indexed
                   (fn [i {:keys [title url snippet date meta]}]
                     (str (inc i) ". **" title "**"
                          (when (seq date) (str " (" date ")"))
                          (when (seq url) (str "\n   " url))
                          (when (seq snippet) (str "\n   " snippet))
                          (when (seq meta) (str "\n   " meta))))
                   posts)))
          (str "# Posts by " username "\n\nNo posts found."))))))

(defn create-thread [forum-url title message]
  (let [base-url (str/replace forum-url #"/+$" "")
        form-url (str base-url "/post-thread")
        ;; Fetch the post-thread form page to get the CSRF token
        resp (get-page form-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching thread creation page") {:url form-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          csrf (or (extract-csrf (:body resp))
                   (throw (ex-info "No CSRF token on page - are you logged in?" {})))
          ;; Find the form action URL on the post-thread page
          thread-form (or (.selectFirst doc "form[action*=post-thread]")
                          (.selectFirst doc "form.block"))
          post-url (if thread-form
                     (let [action (.attr thread-form "action")]
                       (if (or (str/blank? action) (= action "#"))
                         form-url
                         (if (str/starts-with? action "http")
                           action
                           (str "https://www.burbuja.info" action))))
                     form-url)
          ;; POST the new thread
          create-resp (post-form post-url
                        {"title" title
                         "message" message
                         "_xfToken" csrf
                         "_xfRequestUri" base-url
                         "_xfNoRedirect" "1"
                         "_xfResponseType" "json"}
                        :ajax? true)]
      (if (<= 200 (:status create-resp) 303)
        ;; Try to extract the redirect URL from the JSON response
        (let [body (:body create-resp)
              redirect (try
                         (when (seq body)
                           (let [parsed (json/read-str body :key-fn keyword)]
                             (or (:redirect parsed)
                                 (get-in parsed [:visitor :_redirectTarget]))))
                         (catch Exception _ nil))]
          (if redirect
            (str "Thread created successfully.\nURL: "
                 (if (str/starts-with? redirect "http")
                   redirect
                   (str "https://www.burbuja.info" redirect)))
            (str "Thread created successfully.")))
        (throw (ex-info (str "Thread creation failed: HTTP " (:status create-resp))
                        {:body (subs (or (:body create-resp) "")
                                     0 (min 500 (count (or (:body create-resp) ""))))}))))))

(defn reply-comment [post-url message & {:keys [expected-thread]}]
  (let [resp (get-page post-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching post") {:url post-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          ;; Extract post ID from URL
          post-id (or (second (re-find #"post-(\d+)" post-url))
                      (second (re-find #"posts/(\d+)" post-url))
                      (throw (ex-info "Cannot extract post ID from URL" {:url post-url})))
          ;; Validate thread if expected-thread is provided
          canonical (some-> (.selectFirst doc "link[rel=canonical]") (.attr "href"))
          _ (when (and expected-thread canonical)
              (let [actual-thread (second (re-find #"(.*?/temas/[^/]+/)" canonical))
                    expected-base (second (re-find #"(.*?/temas/[^/]+)" expected-thread))]
                (when (and actual-thread expected-base
                           (not (str/starts-with? actual-thread expected-base)))
                  (throw (ex-info (str "Post " post-id " belongs to a different thread. Expected: " expected-thread " but post is in: " actual-thread)
                                  {:expected expected-thread :actual actual-thread})))))
          ;; Find the post on the page
          post-el (or (.selectFirst doc (str "article[data-content=post-" post-id "]"))
                      (.selectFirst doc (str "#js-post-" post-id))
                      (throw (ex-info (str "Post " post-id " not found on page") {})))
          author (.attr post-el "data-author")
          ;; Extract member ID for proper XenForo quote attribution
          member-id (or (some-> (.selectFirst post-el "a[data-user-id]") (.attr "data-user-id"))
                        (let [href (some-> (.selectFirst post-el "a.avatar") (.attr "href"))]
                          (when href (second (re-find #"\.(\d+)/?$" href))))
                        "")
          ;; Extract content without nested quote blocks (removes "dijo:" / "Hacer clic para expandir..." artifacts)
          raw-wrapper (.selectFirst post-el ".bbWrapper")
          content (when raw-wrapper
                    (let [clone (.clone raw-wrapper)]
                      (doseq [el (.select clone ".bbCodeBlock")]
                        (.remove el))
                      (extract-content clone)))
          ;; Truncate quoted content if very long
          quoted (let [c (or content "")]
                   (if (> (count c) 500)
                     (str (subs c 0 497) "...")
                     c))
          ;; CSRF token
          csrf (or (extract-csrf (:body resp))
                   (throw (ex-info "No CSRF token on page" {})))
          ;; Reply URL from quick-reply form
          reply-form (.selectFirst doc "form.js-quickReply")
          reply-url (cond
                      reply-form
                      (let [action (.attr reply-form "action")]
                        (if (str/starts-with? action "http")
                          action
                          (str "https://www.burbuja.info" action)))

                      :else
                      (let [canonical (some-> (.selectFirst doc "link[rel=canonical]")
                                              (.attr "href"))
                            thread-base (when canonical
                                          (second (re-find #"(.*?/temas/[^/]+/)" canonical)))]
                        (if thread-base
                          (str thread-base "add-reply")
                          (throw (ex-info "Cannot determine reply URL - thread may be locked" {})))))
          ;; Build BBCode with quote
          bbcode (str "[QUOTE=\"" author ", post: " post-id
                      (when (seq member-id) (str ", member: " member-id))
                      "\"]\n"
                      quoted "\n"
                      "[/QUOTE]\n\n"
                      message)
          ;; POST the reply
          reply-resp (post-form reply-url
                       {"message" bbcode
                        "_xfToken" csrf
                        "_xfRequestUri" (or (some-> (.selectFirst doc "link[rel=canonical]")
                                                    (.attr "href"))
                                            post-url)
                        "_xfNoRedirect" "1"
                        "_xfResponseType" "json"}
                       :ajax? true)]
      (if (<= 200 (:status reply-resp) 303)
        (str "Reply posted successfully to post " post-id " by " author ".")
        (throw (ex-info (str "Reply failed: HTTP " (:status reply-resp))
                        {:body (subs (or (:body reply-resp) "")
                                     0 (min 500 (count (or (:body reply-resp) ""))))}))))))
