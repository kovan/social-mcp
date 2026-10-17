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
  (when-not (and (string? url) (not (str/blank? url)))
    (throw (ex-info "Request URL must be a non-blank string" {:url url})))
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

(defn- absolute-url [href]
  (cond
    (str/blank? href) nil
    (str/starts-with? href "http") href
    (str/starts-with? href "/") (str "https://www.burbuja.info" href)
    :else href))

(defn- post-content-link? [^Element a href]
  (and (seq href)
       (not (str/starts-with? href "javascript:"))
       (not (str/includes? href "/inmobiliaria/goto/post"))
       (not (str/includes? href "/inmobiliaria/members/"))
       (not (.hasClass a "bbCodeBlock-sourceJump"))))

(defn- annotate-links! [^Element root]
  (doseq [^Element a (.select root "a[href]")]
    (let [href (absolute-url (.attr a "href"))]
      (when (post-content-link? a href)
        (let [text (str/trim (.text a))]
          (.text a (if (or (str/blank? text) (= text href))
                     href
                     (str text " (" href ")")))))))
  root)

(defn- remove-noncontent-elements! [^Element root]
  ;; XenForo injects JSON phrase dictionaries inside post wrappers for image
  ;; lightboxes. Regex-based tag removal would keep the script body and expose
  ;; that configuration as if it were authored post text.
  (doseq [^Element el (.select root "script, style, noscript, template")]
    (.remove el))
  root)

(defn- extract-content
  "Extract readable text from a .bbWrapper element."
  [^Element wrapper]
  (when wrapper
    (-> (doto (.clone wrapper)
          remove-noncontent-elements!
          annotate-links!)
        (.html)
        (str/replace #"(?i)<br\s*/?>" "\n")
        (str/replace #"(?i)</(?:p|div|li)>" "\n")
        (str/replace #"<[^>]+>" "")
        (str/replace "\n" nl)
        Jsoup/parse
        (.text)
        (str/replace nl "\n")
        (str/replace #"\n{3,}" "\n\n")
        str/trim)))

(defn- post-thanks
  "How many thanks (the Zanx reaction) a post has.

   Read from the schema.org microdata XenForo emits in the message footer,
   which carries the running total. The alert feed only reports individual new
   reactions, so it cannot answer this."
  [^Element article]
  (or (some-> (.selectFirst article ".message-microdata meta[itemprop=userInteractionCount]")
              (.attr "content")
              parse-long)
      0))

(defn- parse-post [^Element article]
  (let [author (.attr article "data-author")
        ;; Thread posts carry data-content="post-N"; conversation messages
        ;; have no data-content at all, only a #convMessage-N anchor. Try
        ;; both rather than assuming one shape.
        post-id (or (second (re-find #"(\d+)$" (.attr article "data-content")))
                    (second (re-find #"(\d+)$"
                              (or (some-> (.selectFirst article "span.u-anchorTarget") (.attr "id")) "")))
                    (.attr article "data-content"))
        date (or (some-> (.selectFirst article "time[datetime]") (.attr "datetime")) "")
        number (or (some-> (.selectFirst article ".message-attribution-opposite a:last-child") (.text)) "")
        wrapper (.selectFirst article ".bbWrapper")
        content (extract-content wrapper)]
    {:author author
     :post-id post-id
     :date date
     :number number
     :thanks (post-thanks article)
     :content (or content "")}))

(defn- format-post [{:keys [author post-id date number thanks content]}]
  (str "### " (when (seq number) (str "#" number " - "))
       author
       (when (seq date) (str " (" date ")"))
       " [post:" post-id "]"
       ;; Omitted at zero: most posts have none and the noise would swamp the
       ;; ones that do.
       (when (and thanks (pos? thanks)) (str " [thanks:" thanks "]"))
       "\n\n"
       content))

;; --- Alerts ---
;;
;; burbuja.info runs XenForo, whose alert feed mixes several kinds of event in
;; one list. Each kind is only identifiable by its Spanish wording, so the
;; classification below is text-based on purpose: there is no type attribute on
;; the markup to key off.
;;
;; The single reaction burbuja has enabled is id 1, named "Zanx" (the 👍 emoji),
;; the forum's spelling of "thanks". The name is read from the alert itself
;; rather than hard-coded, so extra reactions would still be reported.

(def ^:private alert-type-patterns
  [[:quote    "citó tu mensaje"]
   [:reaction "reaccionó a"]
   [:reply    "respondió al tema"]
   [:follow   "te está siguiendo"]])

(def ^:private alert-type-labels
  {:quote "quotes" :reaction "thanks" :reply "thread replies"
   :follow "new followers" :other "other"})

(defn- alert-type [body]
  (or (some (fn [[t pattern]] (when (str/includes? body pattern) t))
            alert-type-patterns)
      :other))

(defn- parse-alert-types
  "Accepts a keyword, a string like \"quote,reaction\", a collection, or nil.
   Returns a set of type keywords; \"all\" expands to every known type."
  [types]
  (let [known (into #{:other} (map first alert-type-patterns))
        raw (cond
              (nil? types) nil
              (keyword? types) [types]
              (string? types) (str/split types #"[,\s]+")
              (sequential? types) types
              :else [types])
        parsed (into #{} (comp (map #(-> % name str/trim str/lower-case))
                               (remove str/blank?)
                               (map keyword))
                     raw)]
    (cond
      (empty? parsed) #{:quote :reaction}
      (contains? parsed :all) known
      :else parsed)))

(defn- reaction-name
  "Name of the reaction in a reaction alert, e.g. \"Zanx\". Read from the
   emoji's alt text, which is the only place it appears unabbreviated."
  [^Element li]
  (or (some-> (.selectFirst li ".reaction img[alt]") (.attr "alt") str/trim not-empty)
      (some-> (.selectFirst li ".reaction-text") (.text) str/trim not-empty)))

(defn- alert-thread-title
  "Thread title carried by an alert.

   Parsed from a cleaned copy of the element rather than from the raw text:
   the timestamp and the reaction badge are appended to the same string, and
   thread titles routinely contain periods (\"Perú - 7.2 en la región...\"), so
   anchoring a regex on punctuation truncates them. Stripping those nodes first
   leaves the title as the tail of the sentence."
  [^Element li]
  (let [clone (.clone li)]
    (doseq [^Element el (.select clone "time, .contentRow-minor, .reaction, .u-srOnly")]
      (.remove el))
    (let [text (-> (.text clone)
                   (str/replace #"\s+" " ")
                   ;; Reply alerts append a fixed XenForo sentence after the
                   ;; title; it is boilerplate, not part of the thread name.
                   (str/replace #"\.?\s*Puede haber más mensajes después de esto\.?" "")
                   str/trim)]
      (some-> (or (second (re-find #"en el tema (.+?)\s*(?:\bcon\b\s*)?\.?\s*$" text))
                  (second (re-find #"respondió al tema (.+?)\s*\.?\s*$" text)))
              str/trim
              (str/replace #"[\s.]+$" "")
              not-empty))))

(def ^:private xf-datetime-format
  ;; XenForo writes the offset without a colon ("+0200"), which the plain ISO
  ;; parser rejects. Accept both spellings.
  (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss[XXX][XX]"))

(defn- alert-epoch-ms
  "When an alert happened, in epoch milliseconds.

   XenForo puts the epoch in data-timestamp; data-time holds the clock time as
   displayed (\"11:57 PM\"), which is not a number. Reading the wrong one is
   silent: parse-long yields nil, the age becomes unknown and the hours filter
   then lets every alert through."
  [^Element li]
  (when-let [^Element t (.selectFirst li "time")]
    (or (some-> (.attr t "data-timestamp") not-empty parse-long (* 1000))
        (try
          (-> (java.time.OffsetDateTime/parse (.attr t "datetime") xf-datetime-format)
              .toInstant
              .toEpochMilli)
          (catch Exception _ nil)))))

(defn list-alerts
  ([] (list-alerts 3 nil))
  ([num-pages max-hours] (list-alerts num-pages max-hours nil))
  ([num-pages max-hours types]
   (let [base "https://www.burbuja.info/inmobiliaria/account/alerts"
         wanted (parse-alert-types types)
         now-ms (System/currentTimeMillis)
         docs (for [p (range 1 (inc num-pages))]
                (let [url (if (= p 1) base (str base "?page=" p))
                      resp (get-page url)]
                  (when (= 200 (:status resp))
                    (Jsoup/parse ^String (:body resp)))))
         alerts (for [doc docs
                      :when doc
                      ^Element li (.select doc "li[data-alert-id]")
                      :let [body (.text li)
                            kind (alert-type body)
                            post-link (some-> (.selectFirst li "a.fauxBlockLink-blockLink")
                                              (.attr "href"))
                            ;; Follows have no post to link to, so fall back to
                            ;; the member profile rather than dropping them.
                            link (or post-link
                                     (some-> (.selectFirst li "a.username") (.attr "href")))
                            user (some-> (.selectFirst li "a.username") (.text))
                            time-el (some-> (.selectFirst li "time") (.attr "data-short"))
                            epoch-ms (alert-epoch-ms li)
                            age-hours (when epoch-ms
                                        (/ (double (- now-ms epoch-ms)) 3600000.0))]
                      :when (contains? wanted kind)
                      :when (seq link)
                      :when (or (nil? max-hours)
                                (nil? age-hours)
                                (<= age-hours max-hours))]
                  {:kind kind
                   :user (or user "?")
                   :time (or time-el "?")
                   :reaction (when (= :reaction kind) (reaction-name li))
                   :thread (alert-thread-title li)
                   :url (if (str/starts-with? link "http")
                          link
                          (str "https://www.burbuja.info" link))})
         alerts (vec alerts)
         by-kind (group-by :kind alerts)
         ;; Stable, meaningful order rather than whatever group-by hands back.
         order (concat (map first alert-type-patterns) [:other])
         sections (for [k order
                        :let [items (get by-kind k)]
                        :when (seq items)]
                    (str "## " (str/capitalize (get alert-type-labels k (name k)))
                         " (" (count items) ")\n\n"
                         (str/join "\n"
                                   (map (fn [{:keys [user time reaction thread url]}]
                                          (str "[" time "] " user
                                               (when reaction (str " (" reaction ")"))
                                               ": " url
                                               (when thread (str "\n    " thread))))
                                        items))))]
     (str "# Alerts (" (count alerts) ")\n\n"
          (if (seq sections)
            (str/join "\n\n" sections)
            "No alerts of the requested types.")))))

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

(defn- canonical-thread-url [href]
  (when (seq href)
    (-> href
        (str/replace #"(?:unread|latest)/?$" "")
        absolute-url)))

(defn- post-id-from-url [post-url]
  (or (second (re-find #"post-(\d+)" post-url))
      (second (re-find #"posts/(\d+)" post-url))
      (throw (ex-info "Cannot extract post ID from URL" {:url post-url}))))

(defn- validate-expected-thread! [post-id canonical expected-thread]
  (when (and expected-thread canonical)
    (let [actual-thread (second (re-find #"(.*?/temas/[^/]+/)" canonical))
          expected-base (second (re-find #"(.*?/temas/[^/]+)" expected-thread))]
      (when (and actual-thread expected-base
                 (not (str/starts-with? actual-thread expected-base)))
        (throw (ex-info (str "Post " post-id " belongs to a different thread. Expected: "
                             expected-thread " but post is in: " actual-thread)
                        {:expected expected-thread :actual actual-thread}))))))

(defn- form-action-url [^Element form fallback-url]
  (let [action (some-> form (.attr "action"))]
    (if (str/blank? action)
      fallback-url
      (absolute-url action))))

(defn- input-value [^Element form name]
  (some-> (.selectFirst form (str "input[name=" name "]")) (.attr "value")))

(defn- pair-value [^Element item label]
  (some (fn [^Element pair]
          (when (= label (str/trim (some-> (.selectFirst pair "dt") (.text))))
            (str/trim (or (some-> (.selectFirst pair "dd") (.text)) ""))))
        (.select item ".structItem-cell--meta dl.pairs")))

(defn- parse-forum-thread [^Element item]
  (let [title-link (.selectFirst item ".structItem-title a[href*=/temas/]")
        href (some-> title-link (.attr "href"))
        started-time (.selectFirst item ".structItem-startDate time")
        latest-cell (.selectFirst item ".structItem-cell--latest")
        latest-time (some-> latest-cell (.selectFirst "time"))
        latest-author (some-> latest-cell (.selectFirst "a.username") (.text) str/trim)]
    (when (and title-link (seq href))
      {:title (str/trim (.text title-link))
       :url (canonical-thread-url href)
       :author (or (not-empty (str/trim (.attr item "data-author"))) "")
       :started (or (not-empty (some-> started-time (.attr "datetime")))
                    (some-> started-time (.text) str/trim)
                    "")
       :replies (or (pair-value item "Respuestas") "")
       :views (or (pair-value item "Visitas") "")
       :latest (or (not-empty (some-> latest-time (.attr "datetime")))
                   (some-> latest-time (.text) str/trim)
                   "")
       :latest-author (or latest-author "")
       :sticky? (some? (.selectFirst item ".structItem-status--sticky"))
       :locked? (some? (.selectFirst item ".structItem-status--locked"))
       :unread? (.hasClass item "is-unread")})))

(defn list-forum-threads
  "List threads from a burbuja.info forum/subforum page."
  [forum-url & {:keys [page max-results]
                :or {page 1 max-results 50}}]
  (when-not (re-find #"/inmobiliaria/forums/" forum-url)
    (throw (ex-info "forum_url must be a burbuja.info /inmobiliaria/forums/ URL"
                    {:url forum-url})))
  (let [page (max 1 (int page))
        max-results (-> max-results int (max 1) (min 100))
        base-url (str/replace forum-url #"(?:page-\d+)?/?$" "")
        url (if (= page 1)
              (str base-url "/")
              (str base-url "/page-" page))
        resp (get-page url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching forum") {:url url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          forum-title (or (some-> (.selectFirst doc "h1.p-title-value") (.text) str/trim)
                          "Forum")
          current-page (or (some-> (.selectFirst doc ".pageNav-page--current")
                                   (.text) str/trim parse-long)
                           page)
          total-pages (or (some-> (.selectFirst doc ".pageNav-page:last-child a")
                                  (.text) str/trim parse-long)
                          current-page)
          threads (->> (.select doc "div.structItem--thread")
                       (map parse-forum-thread)
                       (filter some?)
                       (take max-results)
                       vec)]
      (str "# " forum-title "\n"
           "Page " current-page " of " total-pages
           " (" (count threads) " threads returned)\n\n"
           (if (seq threads)
             (str/join
               "\n\n"
               (map-indexed
                 (fn [i {:keys [title url author started replies views latest
                                latest-author sticky? locked? unread?]}]
                   (str (inc i) ". "
                        (when sticky? "[STICKY] ")
                        (when locked? "[LOCKED] ")
                        (when unread? "[UNREAD] ")
                        title
                        "\n   " url
                        "\n   Started by " author
                        (when (seq started) (str " — " started))
                        (when (or (seq replies) (seq views))
                          (str "\n   Replies: " replies " — Views: " views))
                        (when (or (seq latest) (seq latest-author))
                          (str "\n   Latest: " latest
                               (when (seq latest-author)
                                 (str " by " latest-author))))))
                 threads))
             "No threads found.")))))

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
          post-id (post-id-from-url post-url)
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

(defn- authenticated-profile-url []
  (let [resp (get-page "https://www.burbuja.info/inmobiliaria/account/")]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching account details") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          href (some-> (.selectFirst doc "a[href*=/members/][href*='.']")
                       (.attr "href"))]
      (when-not (re-find #"/inmobiliaria/members/[^/]+\.\d+/?$" (or href ""))
        (throw (ex-info "No authenticated Burbuja profile found. Log in through Chrome first."
                        {})))
      (absolute-url href))))

(defn my-posts
  "List recent posts by the currently authenticated burbuja.info account."
  [& {:keys [max-results] :or {max-results 20}}]
  (let [max-results (-> max-results int (max 1) (min 100))
        profile-url (authenticated-profile-url)
        url (str (str/replace profile-url #"/?$" "") "/recent-content")
        resp (get-page url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching own recent posts") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          username (or (some-> (.selectFirst doc "h1.p-title-value") (.text)
                               (str/replace #"^Contenido reciente por\s+" "") str/trim)
                       "authenticated user")
          posts (->> (.select doc "li.block-row")
                     (map parse-search-item)
                     (filter #(and (some? %) (seq (:url %))))
                     (take max-results))]
      (if (seq posts)
        (str "# My posts — " username " (" (count posts) " results)\n\n"
             (str/join "\n\n---\n\n"
               (map-indexed
                 (fn [i {:keys [title url snippet date meta]}]
                   (str (inc i) ". **" title "**"
                        (when (seq date) (str " (" date ")"))
                        "\n   " url
                        (when (seq snippet) (str "\n   " snippet))
                        (when (seq meta) (str "\n   " meta))))
                 posts)))
        (str "# My posts — " username "\n\nNo posts found.")))))

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

(defn- quote-from-response [body]
  (try
    (let [parsed (json/read-str body :key-fn keyword)
          quote (:quote parsed)]
      (when (seq quote) quote))
    (catch Exception _ nil)))

(defn- parse-json-response [body]
  (try
    (json/read-str body :key-fn keyword)
    (catch Exception _ nil)))

(defn- summarize-json-value [v]
  (cond
    (string? v) v
    (map? v) (str/join "; " (keep summarize-json-value (vals v)))
    (sequential? v) (str/join "; " (keep summarize-json-value v))
    (some? v) (str v)
    :else nil))

(defn- response-error-message [parsed]
  (some-> (or (:errors parsed) (:error parsed))
          summarize-json-value
          str/trim
          not-empty))

(defn- successful-xenforo-response [body]
  (let [parsed (parse-json-response body)
        error (response-error-message parsed)]
    (cond
      (nil? parsed)
      {:ok? false
       :error "XenForo returned a non-JSON response"}

      (seq error)
      {:ok? false
       :error error}

      (or (= "ok" (:status parsed))
          (seq (:redirect parsed))
          (seq (:message parsed))
          (seq (:html parsed)))
      {:ok? true
       :redirect (:redirect parsed)
       :message (:message parsed)}

      :else
      {:ok? false
       :error (str "Unexpected XenForo response: "
                   (subs body 0 (min 500 (count body))))})))

(defn- successful-reply-response [body]
  (successful-xenforo-response body))

(defn- fetch-native-quote [post-id csrf request-uri]
  (let [quote-url
        (str "https://www.burbuja.info/inmobiliaria/posts/" post-id "/quote"
             "?_xfToken=" (URLEncoder/encode csrf "UTF-8")
             "&_xfRequestUri=" (URLEncoder/encode request-uri "UTF-8")
             "&_xfWithData=1"
             "&_xfResponseType=json")
        resp (curl-request quote-url :ajax? true)
        quote (quote-from-response (:body resp))]
    (when-not (and (= 200 (:status resp)) (seq quote))
      (throw (ex-info (str "Could not fetch native XenForo quote for post " post-id)
                      {:status (:status resp)
                       :body (subs (or (:body resp) "")
                                   0 (min 500 (count (or (:body resp) ""))))})))
    quote))

(defn edit-post [post-url message & {:keys [expected-thread]}]
  (let [resp (get-page post-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching post") {:url post-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          post-id (post-id-from-url post-url)
          canonical (some-> (.selectFirst doc "link[rel=canonical]") (.attr "href"))
          _ (validate-expected-thread! post-id canonical expected-thread)
          request-uri (or canonical post-url)
          default-edit-url (str "https://www.burbuja.info/inmobiliaria/posts/" post-id "/edit")
          edit-link (or (some-> (.selectFirst doc (str "a[href*=\"/posts/" post-id "/edit\"]"))
                                (.attr "href")
                                absolute-url)
                        default-edit-url)
          edit-resp (get-page edit-link)]
      (when-not (= 200 (:status edit-resp))
        (throw (ex-info (str "HTTP " (:status edit-resp) " fetching edit form")
                        {:url edit-link})))
      (let [edit-doc (Jsoup/parse ^String (:body edit-resp))
            edit-form (or (.selectFirst edit-doc (str "form[action*=\"/posts/" post-id "/edit\"]"))
                          (.selectFirst edit-doc "form[action*=edit]")
                          (throw (ex-info (str "Edit form for post " post-id " not found. "
                                               "The post may not be editable by the authenticated user.")
                                          {:url edit-link})))
            csrf (or (input-value edit-form "_xfToken")
                     (extract-csrf (:body edit-resp))
                     (throw (ex-info "No CSRF token on edit form" {})))
            edit-url (form-action-url edit-form default-edit-url)
            attachment-hash (input-value edit-form "attachment_hash")
            attachment-hash-combined (input-value edit-form "attachment_hash_combined")
            params (cond-> {"message" message
                            "_xfToken" csrf
                            "_xfRequestUri" request-uri
                            "_xfNoRedirect" "1"
                            "_xfResponseType" "json"}
                     (seq attachment-hash)
                     (assoc "attachment_hash" attachment-hash)

                     (seq attachment-hash-combined)
                     (assoc "attachment_hash_combined" attachment-hash-combined))
            save-resp (post-form edit-url params :ajax? true)]
        (if-not (<= 200 (:status save-resp) 303)
          (throw (ex-info (str "Edit failed: HTTP " (:status save-resp))
                          {:body (subs (or (:body save-resp) "")
                                       0 (min 500 (count (or (:body save-resp) ""))))}))
          (let [{:keys [ok? error redirect]} (successful-xenforo-response (:body save-resp))]
            (if ok?
              (str "Post " post-id " edited successfully."
                   (when (seq redirect)
                     (str " " (absolute-url redirect))))
              (throw (ex-info (str "Edit failed: " error)
                              {:body (subs (or (:body save-resp) "")
                                           0 (min 500 (count (or (:body save-resp) ""))))})))))))))

(defn- has-own-quote? [message]
  (boolean (re-find #"(?i)\[quote[\]=\s]" (or message ""))))

(defn- reply-bbcode
  "Prepends XenForo's native quote of the replied post, unless the message
  already carries its own [QUOTE] block (e.g. a partial quote); prepending
  in that case would quote the post twice."
  [native-quote-fn message]
  (if (has-own-quote? message)
    message
    (str (native-quote-fn) "\n\n" message)))

(defn reply-comment [post-url message & {:keys [expected-thread]}]
  (let [resp (get-page post-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching post") {:url post-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          ;; Extract post ID from URL
          post-id (post-id-from-url post-url)
          ;; Validate thread if expected-thread is provided
          canonical (some-> (.selectFirst doc "link[rel=canonical]") (.attr "href"))
          _ (validate-expected-thread! post-id canonical expected-thread)
          ;; Find the post on the page
          post-el (or (.selectFirst doc (str "article[data-content=post-" post-id "]"))
                      (.selectFirst doc (str "#js-post-" post-id))
                      (throw (ex-info (str "Post " post-id " not found on page") {})))
          author (.attr post-el "data-author")
          ;; CSRF token
          csrf (or (extract-csrf (:body resp))
                   (throw (ex-info "No CSRF token on page" {})))
          request-uri (or canonical post-url)
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
          ;; Append the reply to XenForo's exact native quote block, fetched
          ;; as original BBCode rather than rebuilt from rendered HTML. This
          ;; preserves formatting and avoids embedded viewer configuration
          ;; such as js-extraPhrases/lightbox JSON.
          bbcode (reply-bbcode #(fetch-native-quote post-id csrf request-uri)
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
      (if-not (<= 200 (:status reply-resp) 303)
        (throw (ex-info (str "Reply failed: HTTP " (:status reply-resp))
                        {:body (subs (or (:body reply-resp) "")
                                     0 (min 500 (count (or (:body reply-resp) ""))))}))
        (let [{:keys [ok? error redirect]} (successful-reply-response (:body reply-resp))]
          (if ok?
            (str "Reply posted successfully to post " post-id " by " author "."
                 (when (seq redirect)
                   (str " " (absolute-url redirect))))
            (throw (ex-info (str "Reply failed: " error)
                            {:body (subs (or (:body reply-resp) "")
                                         0 (min 500 (count (or (:body reply-resp) ""))))}))))))))

;; --- Private conversations ---
;;
;; XenForo calls private messages "conversations". They reuse the same
;; structItem list markup as forum threads and the same message markup as
;; thread posts, so the parsing below stays close to list-forum-threads and
;; read-thread rather than inventing new shapes.

(defn- parse-conversation-item [^Element item]
  ;; Unlike forum threads, the title is the anchor itself
  ;; (a.structItem-title), not an anchor nested inside a title wrapper.
  (let [title-link (.selectFirst item "a.structItem-title")
        href (some-> title-link (.attr "href"))
        latest-time (.selectFirst item ".structItem-cell--latest time")
        participants (->> (.select item ".structItem-cell--main .structItem-parts a.username")
                          (map #(str/trim (.text ^Element %)))
                          (str/join ", "))]
    (when (and title-link (seq href))
      {:title (str/trim (.text title-link))
       :url (canonical-thread-url href)
       :participants participants
       :latest (or (not-empty (some-> latest-time (.attr "datetime")))
                   (some-> latest-time (.text) str/trim)
                   "")
       :unread? (.hasClass item "is-unread")})))

(defn list-conversations []
  (let [resp (get-page "https://www.burbuja.info/inmobiliaria/conversations/")]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching conversations") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          items (let [primary (.select doc "div.structItem--conversation")]
                  (if (.isEmpty primary) (.select doc "li.structItem--conversation") primary))
          conversations (->> items
                             (map parse-conversation-item)
                             (filter some?)
                             vec)]
      (str "# Private conversations (" (count conversations) ")\n\n"
           (if (seq conversations)
             (str/join "\n\n"
               (map-indexed
                 (fn [i {:keys [title url participants latest unread?]}]
                   (str (inc i) ". " (when unread? "[UNREAD] ") title
                        "\n   " url
                        (when (seq participants) (str "\n   With: " participants))
                        (when (seq latest) (str "\n   Latest: " latest))))
                 conversations))
             "No conversations found.")))))

(def ^:private conversation-message-selector
  "article.message--conversationMessage, article[data-content^=message-]")

(defn read-conversation [conversation-url]
  (let [resp (get-page conversation-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching conversation") {:url conversation-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          title (or (some-> (.selectFirst doc "h1.p-title-value") (.text)) "Untitled")
          articles (let [primary (.select doc conversation-message-selector)]
                     (if (.isEmpty primary) (.select doc "article.message") primary))
          posts (mapv parse-post articles)
          last-page-el (.selectFirst doc ".pageNav-page:last-child a")
          total-pages (or (some-> last-page-el (.text) parse-long) 1)
          current-page-el (.selectFirst doc ".pageNav-page--current a, .pageNav-page--current span")
          current-page (or (some-> current-page-el (.text) parse-long) 1)]
      (str "# " title "\n"
           "Page " current-page " of " total-pages
           " (" (count posts) " messages on this page)\n\n"
           (str/join "\n\n---\n\n" (map format-post posts))))))

(defn reply-conversation [conversation-url message]
  (let [resp (get-page conversation-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching conversation") {:url conversation-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          csrf (or (extract-csrf (:body resp))
                   (throw (ex-info "No CSRF token on page" {})))
          canonical (some-> (.selectFirst doc "link[rel=canonical]") (.attr "href"))
          ;; Read the form's own action rather than hardcoding the reply
          ;; route, same as reply-comment does for threads.
          reply-form (or (.selectFirst doc "form.js-quickReply")
                        (.selectFirst doc "form[action*=insert-reply]")
                        (throw (ex-info "Cannot find reply form on conversation page" {:url conversation-url})))
          reply-url (form-action-url reply-form conversation-url)
          reply-resp (post-form reply-url
                       {"message" message
                        "_xfToken" csrf
                        "_xfRequestUri" (or canonical conversation-url)
                        "_xfNoRedirect" "1"
                        "_xfResponseType" "json"}
                       :ajax? true)]
      (if-not (<= 200 (:status reply-resp) 303)
        (throw (ex-info (str "Reply failed: HTTP " (:status reply-resp))
                        {:body (subs (or (:body reply-resp) "")
                                     0 (min 500 (count (or (:body reply-resp) ""))))}))
        (let [{:keys [ok? error redirect]} (successful-xenforo-response (:body reply-resp))]
          (if ok?
            (str "Reply posted successfully to conversation."
                 (when (seq redirect)
                   (str " " (absolute-url redirect))))
            (throw (ex-info (str "Reply failed: " error)
                            {:body (subs (or (:body reply-resp) "")
                                         0 (min 500 (count (or (:body reply-resp) ""))))}))))))))
