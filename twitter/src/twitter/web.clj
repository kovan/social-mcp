(ns twitter.web
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [twitter.cdp :as cdp]))

(def ^:private base-url "https://x.com")
(def ^:private own-screen-name "shankara_pillai")

(defn- js-string [s]
  (json/write-str (or s "")))

(defn- current-url-js []
  "window.location.href")

(defn- login-state-js []
  "Boolean(document.querySelector('[data-testid=\"SideNav_AccountSwitcher_Button\"]') || document.querySelector('[data-testid=\"AppTabBar_Profile_Link\"]'))")

(defn- composer-ready-js []
  "Boolean(document.querySelector('[data-testid=\"tweetTextarea_0\"]'))")

(defn- focus-composer-js []
  (str
   "(() => {"
   "const el = document.querySelector('[data-testid=\"tweetTextarea_0\"]');"
   "if (!el) return false;"
   "el.focus();"
   "const selection = window.getSelection();"
   "const range = document.createRange();"
   "range.selectNodeContents(el);"
   "selection.removeAllRanges();"
   "selection.addRange(range);"
   "return true;"
   "})()"))

(defn- composer-text-js []
  "(() => { const el = document.querySelector('[data-testid=\"tweetTextarea_0\"]'); return el ? el.innerText : ''; })()")

(defn- post-button-state-js []
  (str
   "(() => Array.from(document.querySelectorAll('[data-testid=\"tweetButton\"], [data-testid=\"tweetButtonInline\"]'))"
   ".map((b,i)=>({i, testid:b.getAttribute('data-testid'), text:b.innerText, disabled:!!b.disabled, aria:b.getAttribute('aria-disabled'), visible:b.offsetParent!==null}))"
   ")()"))

(defn- set-composer-text! [conn text]
  (when-not (cdp/eval! conn (focus-composer-js))
    (throw (ex-info "Could not focus tweet composer" {})))
  (cdp/call! conn "Input.insertText" {:text text})
  (Thread/sleep 1000)
  (let [actual (str/trim (or (cdp/eval! conn (composer-text-js)) ""))]
    (when-not (= actual (str/trim text))
      (throw (ex-info "Could not fill tweet composer"
                      {:actual actual
                       :buttons (cdp/eval! conn (post-button-state-js))}))))
  true)

(defn- click-post-js []
  (str
   "(() => {"
   "const editor = document.querySelector('[data-testid=\"tweetTextarea_0\"]');"
   "const dialog = editor && (editor.closest('[role=\"dialog\"]') || editor.closest('main') || document);"
   "const buttons = Array.from((dialog || document).querySelectorAll('[data-testid=\"tweetButton\"], [data-testid=\"tweetButtonInline\"]'));"
   "const btn = buttons.find(b => !b.disabled && b.getAttribute('aria-disabled') !== 'true');"
   "if (!btn) return false;"
   "btn.click();"
   "return true;"
   "})()"))

(defn- post-button-center-js []
  (str
   "(() => {"
   "const editor = document.querySelector('[data-testid=\"tweetTextarea_0\"]');"
   "const dialog = editor && (editor.closest('[role=\"dialog\"]') || editor.closest('main') || document);"
   "const buttons = Array.from((dialog || document).querySelectorAll('[data-testid=\"tweetButton\"], [data-testid=\"tweetButtonInline\"]'));"
   "const btn = buttons.find(b => !b.disabled && b.getAttribute('aria-disabled') !== 'true' && b.offsetParent !== null);"
   "if (!btn) return null;"
   "const r = btn.getBoundingClientRect();"
   "return {x: r.x + r.width / 2, y: r.y + r.height / 2};"
   "})()"))

(defn- click-post! [conn]
  (if-let [{:keys [x y]} (cdp/eval! conn (post-button-center-js))]
    (let [x (double x)
          y (double y)]
      (cdp/call! conn "Input.dispatchMouseEvent" {:type "mouseMoved"
                                                  :x x
                                                  :y y
                                                  :button "none"})
      (cdp/call! conn "Input.dispatchMouseEvent" {:type "mousePressed"
                                                  :x x
                                                  :y y
                                                  :button "left"
                                                  :buttons 1
                                                  :clickCount 1})
      (cdp/call! conn "Input.dispatchMouseEvent" {:type "mouseReleased"
                                                  :x x
                                                  :y y
                                                  :button "left"
                                                  :buttons 0
                                                  :clickCount 1})
      true)
    (cdp/eval! conn (click-post-js))))

(defn- visible-error-js []
  (str
   "(() => {"
   "const texts = Array.from(document.querySelectorAll('[role=\"alert\"], [data-testid=\"toast\"], [aria-live=\"assertive\"], [aria-live=\"polite\"]'))"
   ".map(e => e.innerText).filter(Boolean);"
   "return texts.join('\\n').slice(0, 1000);"
   "})()"))

(defn- create-tweet-request-id [conn start-index method-name]
  (->> (drop start-index (cdp/events conn))
       (filter #(= "Network.responseReceived" (:method %)))
       (filter (fn [{:keys [params]}]
                 (let [url (get-in params [:response :url])]
                   (and (string? url)
                        (str/includes? url "/i/api/graphql/")
                        (str/includes? url (str "/" method-name))))))
       last
       :params
       :requestId))

(defn- request-finished? [conn start-index request-id]
  (some (fn [{:keys [method params]}]
          (and (= method "Network.loadingFinished")
               (= request-id (:requestId params))))
        (drop start-index (cdp/events conn))))

(defn- network-summary [conn start-index]
  (->> (drop start-index (cdp/events conn))
       (keep (fn [{:keys [method params]}]
               (case method
                 "Network.requestWillBeSent"
                 (let [url (get-in params [:request :url])]
                   (when (and (string? url)
                              (or (str/includes? url "/i/api/")
                                  (str/includes? url "/i/api/graphql/")))
                     (str "REQ " (get-in params [:request :method]) " " url)))
                 "Network.responseReceived"
                 (let [url (get-in params [:response :url])]
                   (when (and (string? url)
                              (or (str/includes? url "/i/api/")
                                  (str/includes? url "/i/api/graphql/")))
                     (str "RES " (get-in params [:response :status]) " " url)))
                 nil)))
       distinct
       (take-last 12)
       (str/join "\n")))

(defn- wait-for-create-response [conn start-index method-name timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [request-id (create-tweet-request-id conn start-index method-name)]
        (do
          (loop [n 0]
            (when (and (< n 20)
                       (not (request-finished? conn start-index request-id)))
              (Thread/sleep 250)
              (recur (inc n))))
          (let [body (try
                       (get-in (cdp/call! conn "Network.getResponseBody" {:requestId request-id})
                               [:body])
                       (catch Exception _ nil))]
            (when (seq body)
              (try (json/read-str body :key-fn keyword)
                   (catch Exception _ {:raw body})))))
        (when (< (System/currentTimeMillis) deadline)
          (Thread/sleep 500)
          (recur))))))

(defn- tweet-id-from-create-response [data method-name]
  (get-in data [:data (keyword (if (= method-name "CreateTweet")
                                 "create_tweet"
                                 "create_retweet"))
                :tweet_results :result :rest_id]))

(defn- status-id-from-url [url]
  (second (re-find #"/status/(\d+)" (or url ""))))

(defn- latest-profile-match-js [text]
  (str
   "(() => {"
   "const wanted = " (js-string (str/trim text)) ";"
   "const norm = s => (s || '').replace(/\\s+/g, ' ').trim();"
   "const articles = Array.from(document.querySelectorAll('article'));"
   "for (const article of articles) {"
   "  const tweetText = article.querySelector('[data-testid=\"tweetText\"]');"
   "  if (!tweetText) continue;"
   "  const text = norm(tweetText.innerText);"
   "  if (text !== norm(wanted) && !text.endsWith(norm(wanted))) continue;"
   "  const link = Array.from(article.querySelectorAll('a[href*=\"/status/\"]')).find(a => /\\/status\\/\\d+/.test(a.getAttribute('href') || ''));"
   "  if (link) return new URL(link.getAttribute('href'), window.location.origin).href;"
   "}"
   "return null;"
   "})()"))

(defn- confirm-from-page [conn url text]
  (cdp/navigate! conn url)
  (Thread/sleep 5000)
  (let [url (cdp/eval! conn (latest-profile-match-js text))]
    (when-let [tweet-id (status-id-from-url url)]
      {:tweet-id tweet-id
       :url url
       :data {:data {:create_tweet {:tweet_results {:result {:rest_id tweet-id}}}}}})))

(defn- confirm-after-post [conn text reply-to-id]
  (or (when reply-to-id
        (confirm-from-page conn (str base-url "/i/status/" reply-to-id) text))
      (confirm-from-page conn (str base-url "/" own-screen-name) text)
      (confirm-from-page conn (str base-url "/" own-screen-name "/with_replies") text)))

(defn- ensure-login! [conn]
  (cdp/navigate! conn base-url)
  (when-not (cdp/wait-until! conn (login-state-js) 15000)
    (throw (ex-info "Twitter web login not detected. Log in to x.com in Chrome and retry." {}))))

(defn create-tweet-web
  "Post a new tweet or reply through the X web UI and confirm via the browser's CreateTweet response."
  [text & {:keys [reply-to-id]}]
  (when (> (count text) 280)
    (throw (ex-info "Tweet text exceeds 280 characters" {:length (count text)})))
  (let [conn (cdp/init-page! (cdp/open-page!))]
    (try
      (ensure-login! conn)
      (cdp/navigate! conn (if reply-to-id
                            (str base-url "/i/status/" reply-to-id)
                            (str base-url "/compose/post")))
      (when-not (cdp/wait-until! conn (composer-ready-js) 20000)
        (throw (ex-info "Tweet composer did not appear" {:url (cdp/eval! conn (current-url-js))})))
      (set-composer-text! conn text)
      (let [start-index (count (cdp/events conn))]
        (when-not (click-post! conn)
          (throw (ex-info "Could not click tweet button"
                          {:visible-error (cdp/eval! conn (visible-error-js))})))
        (let [data (wait-for-create-response conn start-index "CreateTweet" 30000)
              tweet-id (tweet-id-from-create-response data "CreateTweet")]
          (if tweet-id
            {:data data
             :tweet-id tweet-id
             :url (str "https://x.com/i/status/" tweet-id)}
            (if-let [confirmed (confirm-after-post conn text reply-to-id)]
              confirmed
              (throw (ex-info "Twitter web post was not confirmed"
                              {:body (subs (json/write-str (or data {}))
                                           0 (min 1000 (count (json/write-str (or data {})))))
                               :network (network-summary conn start-index)
                               :url (cdp/eval! conn (current-url-js))
                               :visible-error (cdp/eval! conn (visible-error-js))}))))))
      (finally
        (cdp/close! conn)))))
