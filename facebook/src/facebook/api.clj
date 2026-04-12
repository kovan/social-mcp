(ns facebook.api
  "Facebook internal GraphQL API access using Chrome cookies."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [facebook.cookies :as cookies])
  (:import [java.net URLEncoder URLDecoder]
           [java.io File]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

;; --- State ---
(def ^:private cookie-jar (atom nil))
(def ^:private fb-dtsg (atom nil))
(def ^:private user-id (atom nil))

;; --- Cookie init ---
(defn- init-cookies! []
  (when-not @cookie-jar
    (let [cks (cookies/get-cookies ".facebook.com")
          c-user (->> cks (filter #(= (:name %) "c_user")) first :value)
          f (File/createTempFile "fb-cookies" ".txt")]
      (.deleteOnExit f)
      (with-open [w (io/writer f)]
        (.write w "# Netscape HTTP Cookie File\n")
        (doseq [{:keys [name value path host]} cks]
          (when (seq value)
            (let [decoded (try (URLDecoder/decode value "UTF-8") (catch Exception _ value))]
              (.write w (str host "\t"
                            (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                            (or path "/") "\t"
                            "TRUE\t"
                            "0\t"
                            name "\t"
                            decoded "\n"))))))
      (reset! cookie-jar (.getAbsolutePath f))
      (when c-user (reset! user-id c-user))
      (binding [*out* *err*]
        (println (str "Loaded " (count cks) " Facebook cookies"
                      (when c-user (str " (user: " c-user ")"))))))))

;; --- Raw HTTP ---
(defn- curl-get
  "Authenticated GET request. Returns {:status :body}."
  [url]
  (init-cookies!)
  (let [body-file (File/createTempFile "fb-page" ".html")
        args ["curl" "-sSL" "--compressed"
              "-b" @cookie-jar "-c" @cookie-jar
              "-H" (str "User-Agent: " ua)
              "-H" "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"
              "-H" "Accept-Language: en-US,en;q=0.9"
              "-H" "Sec-Fetch-Dest: document"
              "-H" "Sec-Fetch-Mode: navigate"
              "-H" "Sec-Fetch-Site: none"
              "-H" "Sec-Fetch-User: ?1"
              "-H" "Upgrade-Insecure-Requests: 1"
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              url]
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        _ (str/trim (slurp (.getErrorStream proc)))
        _ (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    {:status (or (parse-long status-str) 0) :body body}))

(defn- curl-post
  "Authenticated POST request with form body. Returns {:status :body :data}."
  [url form-body]
  (init-cookies!)
  (let [body-file (File/createTempFile "fb-api" ".json")
        args ["curl" "-sSL"
              "-b" @cookie-jar "-c" @cookie-jar
              "-X" "POST"
              "-H" (str "User-Agent: " ua)
              "-H" "sec-fetch-site: same-origin"
              "-H" "sec-fetch-mode: cors"
              "-H" "sec-fetch-dest: empty"
              "-H" "content-type: application/x-www-form-urlencoded"
              "-d" form-body
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              url]
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
    {:status (or (parse-long status-str) 0)
     :body body
     :data (try (json/read-str body :key-fn keyword) (catch Exception _ nil))}))

;; --- fb_dtsg extraction ---
(defn- extract-fb-dtsg! []
  (when-not @fb-dtsg
    (binding [*out* *err*] (println "Extracting fb_dtsg token..."))
    (let [resp (curl-get "https://www.facebook.com/")
          body (:body resp)
          token (or
                  ;; Pattern 1: DTSGInitialData
                  (second (re-find #"\"DTSGInitialData\",\[\],\{\"token\":\"([^\"]+)\"" body))
                  ;; Pattern 2: hidden form input
                  (second (re-find #"name=\"fb_dtsg\" value=\"([^\"]+)\"" body))
                  ;; Pattern 3: dtsg in JSON
                  (second (re-find #"\"dtsg\":\{\"token\":\"([^\"]+)\"" body))
                  ;; Pattern 4: broader match
                  (second (re-find #"fb_dtsg\" value=\"([^\"]+)\"" body))
                  ;; Pattern 5: DTSG in script data
                  (second (re-find #"\"DTSGInitData\":\{\"token\":\"([^\"]+)\"" body)))]
      (if token
        (do (reset! fb-dtsg token)
            (binding [*out* *err*] (println "Got fb_dtsg token")))
        (throw (ex-info "Could not extract fb_dtsg - make sure you're logged in to Facebook in Chrome" {}))))))

;; --- GraphQL API ---
(defn- graphql-post
  "Make a Facebook GraphQL API call. Returns parsed JSON data."
  [doc-id variables]
  (extract-fb-dtsg!)
  (let [form-body (str "fb_dtsg=" (URLEncoder/encode @fb-dtsg "UTF-8")
                       "&doc_id=" doc-id
                       "&variables=" (URLEncoder/encode (json/write-str variables) "UTF-8"))
        resp (curl-post "https://www.facebook.com/api/graphql/" form-body)]
    (if (or (= 401 (:status resp)) (= 403 (:status resp)))
      ;; Auto-refresh fb_dtsg on auth failure and retry
      (do
        (binding [*out* *err*] (println "Auth failed, refreshing fb_dtsg..."))
        (reset! fb-dtsg nil)
        (extract-fb-dtsg!)
        (let [form-body2 (str "fb_dtsg=" (URLEncoder/encode @fb-dtsg "UTF-8")
                              "&doc_id=" doc-id
                              "&variables=" (URLEncoder/encode (json/write-str variables) "UTF-8"))
              resp2 (curl-post "https://www.facebook.com/api/graphql/" form-body2)]
          (when (not= 200 (:status resp2))
            (throw (ex-info (str "Facebook GraphQL failed after retry: HTTP " (:status resp2))
                            {:body (subs (:body resp2) 0 (min 500 (count (:body resp2))))})))
          (:data resp2)))
      ;; Normal response
      (do
        (when (not= 200 (:status resp))
          (throw (ex-info (str "Facebook GraphQL failed: HTTP " (:status resp))
                          {:body (subs (:body resp) 0 (min 500 (count (:body resp))))})))
        (:data resp)))))

;; --- Extra tokens extracted from HTML ---
(def ^:private lsd-token (atom nil))
(def ^:private jazoest (atom nil))

(defn- extract-tokens!
  "Extract fb_dtsg, lsd, jazoest from Facebook HTML.
   Facebook pre-renders feed data in the initial HTML load."
  []
  (when-not @fb-dtsg
    (binding [*out* *err*] (println "Extracting Facebook tokens..."))
    (let [resp (curl-get "https://www.facebook.com/")
          body (:body resp)
          dtsg (or (second (re-find #"\"DTSGInitialData\",\[\],\{\"token\":\"([^\"]+)\"" body))
                   (second (re-find #"\"dtsg\":\{\"token\":\"([^\"]+)\"" body)))
          lsd (second (re-find #"\"LSD\",\[\],\{\"token\":\"([^\"]+)\"" body))
          jaz (second (re-find #"jazoest=(\d+)" body))]
      (if dtsg
        (do (reset! fb-dtsg dtsg)
            (when lsd (reset! lsd-token lsd))
            (when jaz (reset! jazoest jaz))
            (binding [*out* *err*] (println "Got Facebook tokens")))
        (throw (ex-info "Could not extract fb_dtsg - make sure you're logged in to Facebook in Chrome" {}))))))

;; --- Feed extraction from pre-rendered HTML ---
(defn- extract-feed-from-html
  "Extract feed posts from Facebook's pre-rendered HTML data.
   Facebook embeds relay data as JSON in the initial page load."
  [html]
  (let [marker "\"is_text_only_story\""
        results (atom [])
        seen (atom #{})]
    (loop [search-from 0]
      (let [idx (.indexOf html marker (int search-from))]
        (when (>= idx 0)
          ;; Get a window around this story marker
          (let [window (subs html idx (min (count html) (+ idx 2000)))
                ;; Extract message text - use a pattern that matches across nested braces
                text-match (re-find #"\"text\":\"((?:[^\"\\]|\\.){10,})\"" window)]
            (when text-match
              (let [raw-text (second text-match)
                    key (subs raw-text 0 (min 30 (count raw-text)))]
                (when-not (contains? @seen key)
                  (swap! seen conj key)
                  ;; Look backwards for author name
                  (let [before-start (max 0 (- idx 3000))
                        before (subs html before-start idx)
                        names (->> (re-seq #"\"name\":\"([^\"]{2,60})\"" before)
                                   (map second)
                                   (remove #(contains? #{"display" "custom" "everyone"
                                                          "CometFeedStory" "left" "right"} %)))
                        author (or (last names) "?")]
                    (swap! results conj
                      {:author author
                       :text raw-text}))))))
          (recur (+ idx (count marker))))))
    @results))

(defn- consent-flow-blocked?
  "Facebook sometimes serves an ads-consent interstitial instead of the feed.
   Detect that so the caller gets a useful error instead of an empty post list."
  [html]
  (and (str/includes? html "\"launch_consent_flow\"")
       (or (str/includes? html "ConsentAPIPresentPromptAction")
           (str/includes? html "procesar tus datos para los anuncios")
           (str/includes? html "process your data for ads"))))

;; --- Public API functions ---

(defn init!
  "Initialize cookies and extract fb_dtsg. Call this to verify auth works."
  []
  (init-cookies!)
  (extract-tokens!)
  {:user-id @user-id :fb-dtsg (subs @fb-dtsg 0 (min 10 (count @fb-dtsg)))})

(defn news-feed
  "Get news feed posts from the pre-rendered Facebook HTML."
  [n]
  (init-cookies!)
  (let [resp (curl-get "https://www.facebook.com/")
        body (:body resp)]
    (when (consent-flow-blocked? body)
      (throw (ex-info
              "Facebook is showing an ads-consent prompt instead of the feed. Open facebook.com once in Chrome, finish or dismiss that prompt, then retry."
              {:kind :consent-prompt})))
    ;; Also extract tokens while we're at it
    (when-not @fb-dtsg
      (let [dtsg (or (second (re-find #"\"DTSGInitialData\",\[\],\{\"token\":\"([^\"]+)\"" body))
                     (second (re-find #"\"dtsg\":\{\"token\":\"([^\"]+)\"" body)))]
        (when dtsg (reset! fb-dtsg dtsg))))
    (take n (extract-feed-from-html body))))

(defn page-posts
  "Get posts from a specific Facebook page."
  [page-name n]
  (init-cookies!)
  (let [url (str "https://www.facebook.com/" (str/replace page-name #"^@" "") "/")
        resp (curl-get url)
        body (:body resp)]
    (when (consent-flow-blocked? body)
      (throw (ex-info
              "Facebook is showing an ads-consent prompt instead of page content. Open facebook.com once in Chrome, finish or dismiss that prompt, then retry."
              {:kind :consent-prompt})))
    (take n (extract-feed-from-html body))))
