(ns amazon.http
  (:require [amazon.cookies :as cookies]
            [clojure.string :as str])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.util.zip GZIPInputStream]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36")

(def ^:private cookie-file (atom nil))

(defn cookie-jar! []
  (or @cookie-file
      (let [f (java.io.File/createTempFile "amazon-cookies" ".txt")]
        (.deleteOnExit f)
        (cookies/write-netscape-cookie-file! (.getAbsolutePath f) (cookies/get-cookies))
        (reset! cookie-file (.getAbsolutePath f)))))

(defn encode-query [params]
  (str/join "&"
            (for [[k v] params
                  :when (some? v)]
              (str (URLEncoder/encode (str k) "UTF-8")
                   "="
                   (URLEncoder/encode (str v) "UTF-8")))))

(defn- gzip? [bytes]
  (and (<= 2 (alength bytes))
       (= 0x1f (bit-and 0xff (aget bytes 0)))
       (= 0x8b (bit-and 0xff (aget bytes 1)))))

(defn- gunzip [bytes]
  (with-open [in (GZIPInputStream. (ByteArrayInputStream. bytes))
              out (ByteArrayOutputStream.)]
    (.transferTo in out)
    (.toByteArray out)))

(defn- read-body [file]
  (let [bytes (Files/readAllBytes (.toPath file))
        decoded (if (gzip? bytes) (gunzip bytes) bytes)]
    (String. decoded StandardCharsets/UTF_8)))

(defn request
  [url & {:keys [method params headers]}]
  (let [body-file (java.io.File/createTempFile "amazon-body" ".html")
        base ["curl" "-sSL"
              "--compressed"
              "-b" (cookie-jar!) "-c" (cookie-jar!)
              "-H" (str "User-Agent: " ua)
              "-H" "Accept-Language: es-ES,es;q=0.9,en;q=0.8"
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"]
        args (cond-> base
               (= method :post) (into ["-X" "POST"])
               (seq headers) (into (mapcat (fn [[k v]] ["-H" (str k ": " v)]) headers))
               (seq params) (into ["-d" (encode-query params)])
               true (conj url))
        proc (.start (ProcessBuilder. ^java.util.List args))
        status-str (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)
        body (read-body body-file)]
    (.delete body-file)
    (when-not (zero? exit)
      (throw (ex-info (str "curl failed: " err) {:exit exit})))
    {:status (or (parse-long status-str) 0)
     :body body}))
