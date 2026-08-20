(ns twitter.cdp
  (:require [clojure.data.json :as json]
            [twitter.cookies :as cookies])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse HttpResponse$BodyHandlers WebSocket WebSocket$Listener]
           [java.util.concurrent CompletableFuture TimeUnit]))

(def ^:private chrome "/opt/google/chrome/chrome")
(def ^:private port 9232)
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
    (cond
      (try
        (http-json (str "http://127.0.0.1:" port "/json/version"))
        true
        (catch Exception _ false))
      true

      (< n 50)
      (do
        (Thread/sleep 200)
        (recur (inc n)))

      :else
      (throw (ex-info "Chrome DevTools endpoint did not start" {:port port})))))

(defn ensure-chrome! []
  (when-not (and @chrome-proc (.isAlive @chrome-proc))
    (let [dir (doto (java.io.File. "/home/k/.cache/social-mcp/twitter-chrome") .mkdirs)
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

(defrecord Listener [responses events buffer]
  WebSocket$Listener
  (onText [_ ws data last?]
    (swap! buffer str data)
    (when last?
      (try
        (let [payload @buffer
              _ (reset! buffer "")
              msg (json/read-str payload :key-fn keyword)]
          (if-let [id (:id msg)]
            (when-let [f (get @responses id)]
              (.complete ^CompletableFuture f msg)
              (swap! responses dissoc id))
            (when-let [method (:method msg)]
              (swap! events (fn [xs]
                              (let [ys (conj (or xs []) {:method method :params (:params msg)})]
                                (if (> (count ys) 250) (subvec (vec ys) (- (count ys) 250)) ys)))))))
        (catch Exception _)))
    (.request ws 1)
    nil))

(defn open-page! []
  (ensure-chrome!)
  (let [target (http-json (str "http://127.0.0.1:" port "/json/new?about:blank") :put)
        responses (atom {})
        events (atom [])
        buffer (atom "")
        ids (atom 0)
        listener (->Listener responses events buffer)
        ws (-> client
               (.newWebSocketBuilder)
               (.buildAsync (URI/create (:webSocketDebuggerUrl target)) listener)
               (.get 10 TimeUnit/SECONDS))]
    (.request ws 1)
    {:ws ws :responses responses :events events :ids ids}))

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
  (let [cks (for [{:keys [name value host path]} (distinct (concat
                                                            (cookies/get-cookies ".x.com")
                                                            (cookies/get-cookies ".twitter.com")))]
              {:name name
               :value value
               :domain host
               :path (or path "/")
               :secure true})]
    (call! conn "Network.setCookies" {:cookies (vec cks)}))
  conn)

(defn navigate! [conn url]
  (call! conn "Page.navigate" {:url url})
  (Thread/sleep 4500)
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

(defn events [conn]
  @(:events conn))

;; --- JS bundle discovery ---
;; X stopped emitting <script src="..."> in the x.com HTML, so scraping the
;; markup finds no bundles at all. The browser still loads them, so we ask the
;; browser what it loaded instead of guessing.

(def ^:private bundle-routes
  ;; X code-splits per route: the chunk that defines TweetDetail is not the one
  ;; that defines HomeTimeline, so we have to visit each area once.
  ["https://x.com/home"
   "https://x.com/search?q=test&src=typed_query"
   "https://x.com/jack/status/20"])

;; Installed before every navigation. X clears the Performance resource-timing
;; buffer and removes chunk <script> tags after they load, so neither
;; performance.getEntriesByType nor document.scripts sees the route chunks.
;; Intercepting the src setter catches every bundle at the moment it is injected.
(def ^:private bundle-hook-js
  "(function(){
     if (window.__bundlesHooked) return;
     window.__bundlesHooked = true;
     window.__bundles = [];
     var push = function(v){ try { if (v) window.__bundles.push(String(v)); } catch(e){} };
     var origCreate = document.createElement.bind(document);
     document.createElement = function(tag){
       var el = origCreate(tag);
       if (String(tag).toLowerCase() === 'script') {
         try {
           var d = Object.getOwnPropertyDescriptor(HTMLScriptElement.prototype, 'src');
           Object.defineProperty(el, 'src', {
             configurable: true,
             get: function(){ return d.get.call(this); },
             set: function(v){ push(v); d.set.call(this, v); }
           });
         } catch(e){}
       }
       return el;
     };
   })();")

(def ^:private collect-js
  "(function(){
     var out = (window.__bundles || []).slice();
     try { Array.prototype.slice.call(document.scripts)
             .forEach(function(s){ if (s.src) out.push(s.src); }); } catch(e){}
     try { performance.getEntriesByType('resource')
             .forEach(function(e){ out.push(e.name); }); } catch(e){}
     return out.filter(function(n){
       return n.indexOf('abs.twimg.com/responsive-web/') !== -1
           && n.indexOf('.js') !== -1;
     });
   })()")

(defn script-urls!
  "Open X in the real browser and return every JS bundle URL it actually loads.
   Visits several routes so route-split chunks are included."
  []
  (let [conn (open-page!)]
    (try
      (init-page! conn)
      (call! conn "Page.addScriptToEvaluateOnNewDocument" {:source bundle-hook-js})
      (->> bundle-routes
           (mapcat (fn [route]
                     (try
                       (navigate! conn route)
                       ;; give lazy route chunks time to be requested
                       (wait-until! conn
                                    "(window.__bundles||[]).length > 2"
                                    15000)
                       (or (eval! conn collect-js) [])
                       (catch Exception e
                         (binding [*out* *err*]
                           (println "CDP route failed:" route (.getMessage e)))
                         []))))
           distinct
           vec)
      (finally (close! conn)))))

