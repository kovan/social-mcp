(ns facebook.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [facebook.api :as api])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "init"
    :description "Initialize Facebook auth and verify cookies + fb_dtsg extraction works."
    :inputSchema {:type "object"
                  :properties {}}}
   {:name "news_feed"
    :description "Get posts from your Facebook news feed."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of posts to return (default 10)"}}}}
   {:name "page_posts"
    :description "Get recent posts from a Facebook page."
    :inputSchema {:type "object"
                  :properties {:page {:type "string"
                                      :description "Page name or URL (e.g. 'bbcnews' or 'https://facebook.com/bbcnews')"}
                               :n {:type "number"
                                   :description "Number of posts (default 10)"}}
                  :required ["page"]}}])

(defn- respond [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn- error-response [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn- tool-result [text & {:keys [error?]}]
  {:content [{:type "text" :text text}]
   :isError (boolean error?)})

(defn- handle-initialize [id _params]
  (respond id
    {:protocolVersion "2024-11-05"
     :capabilities {:tools {}}
     :serverInfo {:name "facebook-mcp" :version "0.1.0"}
     :instructions "MCP server for Facebook. Uses Chrome cookies for authentication - make sure you're logged in to facebook.com in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "init"
            (let [{:keys [user-id fb-dtsg]} (api/init!)]
              (str "Facebook auth OK. User ID: " user-id ", fb_dtsg: " fb-dtsg "..."))

            "news_feed"
            (let [n (or (:n arguments) 10)
                  posts (api/news-feed n)]
              (if (seq posts)
                (str "# News Feed (" (count posts) " posts)\n\n"
                     (str/join "\n---\n\n"
                       (map-indexed
                         (fn [i {:keys [author text]}]
                           (let [decoded (try (.replaceAll text "\\\\u([0-9a-fA-F]{4})"
                                               (reify java.util.function.Function
                                                 (apply [_ m]
                                                   (str (char (Integer/parseInt (.group m 1) 16))))))
                                              (catch Exception _ text))
                                 clean (subs decoded 0 (min 300 (count decoded)))]
                             (str (inc i) ". **" author "**\n" clean)))
                         posts)))
                "# News Feed\n\nNo posts found."))

            "page_posts"
            (let [page (str/replace (or (:page arguments) "") #"https?://[^/]+/" "")
                  page-name (str/replace page #"/$" "")
                  n (or (:n arguments) 10)
                  posts (api/page-posts page-name n)]
              (if (seq posts)
                (str "# Posts from " page-name " (" (count posts) " posts)\n\n"
                     (str/join "\n---\n\n"
                       (map-indexed
                         (fn [i {:keys [author text]}]
                           (let [decoded (try (.replaceAll text "\\\\u([0-9a-fA-F]{4})"
                                               (reify java.util.function.Function
                                                 (apply [_ m]
                                                   (str (char (Integer/parseInt (.group m 1) 16))))))
                                              (catch Exception _ text))
                                 clean (subs decoded 0 (min 300 (count decoded)))]
                             (str (inc i) ". **" author "**\n" clean)))
                         posts)))
                (str "# Posts from " page-name "\n\nNo posts found.")))

            (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (respond id (tool-result (str "Error: " (.getMessage e)) :error? true)))))

(defn- handle-message [msg]
  (let [{:keys [id method params]} msg]
    (case method
      "initialize"                (handle-initialize id params)
      "notifications/initialized" nil
      "tools/list"                (handle-tools-list id params)
      "tools/call"                (handle-tools-call id params)
      "ping"                      (respond id {})
      (if id
        (error-response id -32601 (str "Method not found: " method))
        nil))))

(defn- write-response [resp]
  (let [out System/out
        line (str (json/write-str resp) "\n")]
    (.write out (.getBytes line "UTF-8"))
    (.flush out)))

(defn -main [& _args]
  (let [reader (BufferedReader. (InputStreamReader. System/in))]
    (loop []
      (when-let [line (.readLine reader)]
        (when-not (str/blank? line)
          (try
            (let [msg (json/read-str line :key-fn keyword)
                  resp (handle-message msg)]
              (when resp
                (write-response resp)))
            (catch Exception e
              (binding [*out* *err*]
                (println "Error processing message:" (.getMessage e))))))
        (recur)))))
