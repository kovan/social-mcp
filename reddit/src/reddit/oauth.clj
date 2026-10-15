(ns reddit.oauth
  "OAuth access to Reddit for a registered script app.
   Enabled when REDDIT_CLIENT_ID is set; also needs REDDIT_CLIENT_SECRET,
   REDDIT_USERNAME and REDDIT_PASSWORD. Requests go to oauth.reddit.com,
   which (unlike www/old.reddit.com) is reachable from cloud IPs."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URLEncoder]
           [java.nio.file Files]
           [java.nio.file.attribute PosixFilePermissions]))

(defn- env [k] (not-empty (System/getenv k)))

(defn enabled? [] (some? (env "REDDIT_CLIENT_ID")))

(defn username [] (env "REDDIT_USERNAME"))

(defn- user-agent []
  ;; Reddit asks for <platform>:<app ID>:<version> (by /u/<username>)
  (str "linux:" (env "REDDIT_CLIENT_ID") ":0.1.0 (by /u/" (username) ")"))

(defn- form-encode [params]
  (str/join "&"
    (map (fn [[k v]]
           (str (URLEncoder/encode (name k) "UTF-8") "="
                (URLEncoder/encode (str v) "UTF-8")))
         params)))

(defn- curl-quote [s]
  (str "\"" (-> s (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) "\""))

(defn- curl
  "Run curl with secrets (auth, headers, body) in a private config file
   instead of argv, so they don't show up in `ps`. Returns {:status :body}."
  [url {:keys [method user headers data]}]
  (let [cfg (.toFile (Files/createTempFile "reddit-curl" ".cfg"
                       (into-array [(PosixFilePermissions/asFileAttribute
                                     (PosixFilePermissions/fromString "rw-------"))])))
        body-file (java.io.File/createTempFile "reddit-resp" ".json")]
    (try
      (with-open [w (io/writer cfg)]
        (when user (.write w (str "user = " (curl-quote user) "\n")))
        (doseq [h (cons (str "User-Agent: " (user-agent)) headers)]
          (.write w (str "header = " (curl-quote h) "\n")))
        (when data (.write w (str "data = " (curl-quote data) "\n"))))
      (let [args (cond-> ["curl" "-sS" "-K" (.getAbsolutePath cfg)
                          "-o" (.getAbsolutePath body-file)
                          "-w" "%{http_code}"]
                   (= method :post) (into ["-X" "POST"])
                   true (conj url))
            proc (.start (ProcessBuilder. ^java.util.List args))
            status (str/trim (slurp (.getInputStream proc)))
            err (str/trim (slurp (.getErrorStream proc)))]
        (.waitFor proc)
        (when (seq err)
          (binding [*out* *err*] (println "curl error:" err)))
        {:status (or (parse-long status) 0)
         :body (slurp body-file)})
      (finally
        (.delete cfg)
        (.delete body-file)))))

(def ^:private token (atom nil))

(defn- access-token []
  (let [{:keys [value expires]} @token]
    (if (and value (< (System/currentTimeMillis) expires))
      value
      (let [{:keys [status body]}
            (curl "https://www.reddit.com/api/v1/access_token"
                  {:method :post
                   :user (str (env "REDDIT_CLIENT_ID") ":" (env "REDDIT_CLIENT_SECRET"))
                   :data (form-encode {:grant_type "password"
                                       :username (username)
                                       :password (env "REDDIT_PASSWORD")})})
            data (try (json/read-str body :key-fn keyword) (catch Exception _ nil))]
        (when-not (and (= status 200) (:access_token data))
          (throw (ex-info (str "OAuth token request failed (HTTP " status "): "
                               (or (:error data) (subs body 0 (min 200 (count body)))))
                          {:status status})))
        (reset! token {:value (:access_token data)
                       ;; refresh a minute before it expires
                       :expires (+ (System/currentTimeMillis)
                                   (* 1000 (- (:expires_in data 3600) 60)))})
        (:access_token data)))))

(defn- oauth-url
  "Point a www/old.reddit.com URL (or a bare path) at oauth.reddit.com."
  [url]
  (str/replace url #"^(https://(www|old)\.reddit\.com)?" "https://oauth.reddit.com"))

(defn request
  "Authenticated request. method is :get or :post; params are form fields.
   Returns {:status :body :data}."
  [method url & [params]]
  (let [{:keys [status body]}
        (curl (oauth-url url)
              {:method method
               :headers [(str "Authorization: bearer " (access-token))]
               :data (when (= method :post) (form-encode params))})]
    {:status status
     :body body
     :data (try (json/read-str body :key-fn keyword) (catch Exception _ nil))}))
