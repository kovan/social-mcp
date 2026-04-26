(ns amazon.cdp
  (:require [amazon.cookies :as cookies]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse HttpResponse$BodyHandlers WebSocket WebSocket$Listener]
           [java.util.concurrent CompletableFuture TimeUnit]))

(def ^:private chrome "/opt/google/chrome/chrome")
(def ^:private port 9224)
(def ^:private client (HttpClient/newHttpClient))
(defonce ^:private chrome-proc (atom nil))

(defn- http-json
  ([url] (http-json url :get))
  ([url method]
   (let [builder (-> (HttpRequest/newBuilder (URI/create url))
                     (.timeout (java.time.Duration/ofSeconds 10)))
         req (case method
               :put (-> builder (.PUT (HttpRequest$BodyPublishers/noBody)) .build)
               (-> builder .GET .build))
         resp (.send client req (HttpResponse$BodyHandlers/ofString))]
     (json/read-str (.body resp) :key-fn keyword))))

(defn- wait-for-chrome []
  (loop [n 0]
    (if (< n 50)
      (if (try
            (http-json (str "http://127.0.0.1:" port "/json/version"))
            true
            (catch Exception _ false))
        true
        (do
          (Thread/sleep 200)
          (recur (inc n))))
      (throw (ex-info "Chrome DevTools endpoint did not start" {:port port})))))

(defn ensure-chrome! []
  (when-not (and @chrome-proc (.isAlive @chrome-proc))
    (let [dir (doto (java.io.File. "/home/k/.cache/social-mcp/amazon-chrome") .mkdirs)
          proc (.start (ProcessBuilder.
                        [chrome
                         (str "--remote-debugging-port=" port)
                         (str "--user-data-dir=" (.getAbsolutePath dir))
                         "--no-first-run"
                         "--no-default-browser-check"
                         "--disable-setuid-sandbox"
                         "--no-sandbox"]))]
      (reset! chrome-proc proc)
      (wait-for-chrome)))
  true)

(defrecord Listener [responses buffer]
  WebSocket$Listener
  (onText [_ ws data last?]
    (swap! buffer str data)
    (when last?
      (try
        (let [payload @buffer
              _ (reset! buffer "")
              msg (json/read-str payload :key-fn keyword)]
          (when-let [id (:id msg)]
            (when-let [f (get @responses id)]
              (.complete ^CompletableFuture f msg)
              (swap! responses dissoc id))))
        (catch Exception _)))
    (.request ws 1)
    nil))

(defn open-page! []
  (ensure-chrome!)
  (let [target (http-json (str "http://127.0.0.1:" port "/json/new?about:blank") :put)
        responses (atom {})
        buffer (atom "")
        ids (atom 0)
        listener (->Listener responses buffer)
        ws (-> client
               (.newWebSocketBuilder)
               (.buildAsync (URI/create (:webSocketDebuggerUrl target)) listener)
               (.get 10 TimeUnit/SECONDS))]
    (.request ws 1)
    {:ws ws :responses responses :ids ids}))

(defn call!
  ([conn method] (call! conn method {}))
  ([{:keys [ws responses ids]} method params]
   (let [id (swap! ids inc)
         f (CompletableFuture.)]
     (swap! responses assoc id f)
     (.get (.sendText ^WebSocket ws (json/write-str {:id id :method method :params params}) true)
           10 TimeUnit/SECONDS)
     (let [resp (.get f 30 TimeUnit/SECONDS)]
       (when-let [err (:error resp)]
         (throw (ex-info (str "CDP error calling " method ": " (:message err)) err)))
       (:result resp)))))

(defn close! [{:keys [ws]}]
  (try (.sendClose ^WebSocket ws WebSocket/NORMAL_CLOSURE "done")
       (catch Exception _)))

(defn init-page! [conn]
  (call! conn "Page.enable")
  (call! conn "Runtime.enable")
  (call! conn "Network.enable")
  (let [cks (for [{:keys [name value host path secure expires]} (cookies/get-cookies)]
              (cond-> {:name name :value value :domain host :path (or path "/") :secure (boolean secure)}
                expires (assoc :expires (double expires))))]
    (call! conn "Network.setCookies" {:cookies (vec cks)}))
  conn)

(defn navigate! [conn url]
  (call! conn "Page.navigate" {:url url})
  (Thread/sleep 5000)
  conn)

(defn eval! [conn expression]
  (get-in (call! conn "Runtime.evaluate"
                 {:expression expression
                  :awaitPromise true
                  :returnByValue true})
          [:result :value]))

(defn wait-until!
  [conn expression timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [v (try (eval! conn expression) (catch Exception _ false))]
        (cond
          v v
          (< (System/currentTimeMillis) deadline) (do (Thread/sleep 500) (recur))
          :else false)))))
