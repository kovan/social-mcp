(ns hn.web
  "Authenticated web interactions with Hacker News (login, comment, submit)."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hn.cookies :as cookies])
  (:import [org.jsoup Jsoup]
           [java.net URLEncoder]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(def ^:private cookie-jar (atom nil))

(defn- init-cookies! []
  (let [cks (cookies/get-cookies "news.ycombinator.com")
        f (java.io.File/createTempFile "hn-cookies" ".txt")]
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
                         value "\n")))))
    (reset! cookie-jar (.getAbsolutePath f))
    (binding [*out* *err*]
      (println (str "Loaded " (count cks) " cookies for news.ycombinator.com")))))

(defn- curl-request [url & {:keys [method params]}]
  (init-cookies!)
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
                             (keep (fn [[k v]]
                                     (when (some? v)
                                       (str (URLEncoder/encode (str k) "UTF-8") "="
                                            (URLEncoder/encode (str v) "UTF-8"))))
                                   params))])

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

(defn- post-form [url params]
  (curl-request url :method :post :params params))

(defn reply-comment
  "Reply to a comment or story on HN. Returns success message."
  [item-id message]
  ;; Try /reply?id= first (works for comments), fall back to /item?id= (for stories)
  (let [reply-url (str "https://news.ycombinator.com/reply?id=" item-id)
        resp (get-page reply-url)
        doc (Jsoup/parse ^String (or (:body resp) ""))
        form (.selectFirst doc "form[action=comment]")
        ;; If no comment form on reply page, try the story page directly
        [doc form] (if form
                     [doc form]
                     (let [item-url (str "https://news.ycombinator.com/item?id=" item-id)
                           resp2 (get-page item-url)
                           doc2 (Jsoup/parse ^String (or (:body resp2) ""))
                           form2 (.selectFirst doc2 "form[action=comment]")]
                       [doc2 form2]))]
    (let [_ (when-not form
              (throw (ex-info "Reply form not found - may need to log in to Chrome" {:id item-id})))
          hmac (some-> (.selectFirst form "input[name=hmac]") (.val))
          parent (some-> (.selectFirst form "input[name=parent]") (.val))
          goto (some-> (.selectFirst form "input[name=goto]") (.val))
          _ (when-not hmac
              (throw (ex-info "HMAC token not found" {})))
          post-resp (post-form "https://news.ycombinator.com/comment"
                      {"parent" (or parent item-id)
                       "goto" (or goto (str "item?id=" item-id))
                       "hmac" hmac
                       "text" message})]
      (if (<= 200 (:status post-resp) 303)
        (str "Reply posted successfully to item " item-id ".")
        (throw (ex-info (str "Reply failed: HTTP " (:status post-resp))
                        {:body (subs (or (:body post-resp) "")
                                     0 (min 500 (count (or (:body post-resp) ""))))}))))))

(defn list-replies
  "Fetch replies to the logged-in user's comments from /threads page."
  []
  (init-cookies!)
  ;; Get username from the nav bar
  (let [resp (get-page "https://news.ycombinator.com/news")
        doc (Jsoup/parse ^String (:body resp))
        user-link (.selectFirst doc "a#me")
        username (when user-link (.text user-link))]
    (when-not (seq username)
      (throw (ex-info "Could not determine username - make sure you're logged in to HN in Chrome" {})))
    (let [threads-resp (get-page (str "https://news.ycombinator.com/threads?id=" username))
          threads-doc (Jsoup/parse ^String (:body threads-resp))
          comments (.select threads-doc "tr.athing.comtr")
          pairs (loop [items (seq comments)
                       result []
                       last-own nil]
                  (if-not items
                    result
                    (let [item (first items)
                          author-el (.selectFirst item "a.hnuser")
                          author (when author-el (.text author-el))
                          is-own (= author username)
                          text-el (or (.selectFirst item "div.commtext")
                                     (.selectFirst item "span.commtext"))
                          text (when text-el (.text text-el))
                          age-el (.selectFirst item "span.age a")
                          age (when age-el (.text age-el))
                          id-attr (.attr item "id")]
                      (if is-own
                        (recur (next items) result
                               {:text (when text (subs text 0 (min 150 (count text))))
                                :id id-attr})
                        (if (and last-own (seq author))
                          (recur (next items)
                                 (conj result
                                       {:reply-author author
                                        :reply-text (or text "")
                                        :reply-age (or age "")
                                        :reply-id id-attr
                                        :own-text (:text last-own)
                                        :own-id (:id last-own)})
                                 nil)
                          (recur (next items) result nil))))))]
      (if (seq pairs)
        (str "# Replies to " username " (" (count pairs) ")\n\n"
             (clojure.string/join "\n---\n\n"
               (map-indexed
                 (fn [i {:keys [reply-author reply-text reply-age reply-id own-text]}]
                   (str (inc i) ". **" reply-author "** (" reply-age ") [item:" reply-id "]\n"
                        (when own-text (str "> " own-text "...\n"))
                        reply-text))
                 pairs)))
        (str "# Replies to " username "\n\nNo replies found.")))))

(defn submit-story
  "Submit a new story to HN. Provide title and either url or text."
  [title & {:keys [url text]}]
  (let [submit-url "https://news.ycombinator.com/submit"
        resp (get-page submit-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching submit page") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          form (.selectFirst doc "form")
          _ (when-not form
              (throw (ex-info "Submit form not found - may need to log in to Chrome" {})))
          fnid (some-> (.selectFirst form "input[name=fnid]") (.val))
          fnop (some-> (.selectFirst form "input[name=fnop]") (.val))
          _ (when-not fnid
              (throw (ex-info "fnid token not found" {})))
          params (cond-> {"fnid" fnid
                          "fnop" (or fnop "submit-page")
                          "title" title}
                   url (assoc "url" url)
                   text (assoc "text" text))
          post-resp (post-form "https://news.ycombinator.com/r" params)]
      (if (<= 200 (:status post-resp) 303)
        (str "Story submitted successfully: " title)
        (throw (ex-info (str "Submit failed: HTTP " (:status post-resp))
                        {:body (subs (or (:body post-resp) "")
                                     0 (min 500 (count (or (:body post-resp) ""))))}))))))
