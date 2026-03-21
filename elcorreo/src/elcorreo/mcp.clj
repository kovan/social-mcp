(ns elcorreo.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [elcorreo.api :as api]
            [elcorreo.format :as fmt])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private vocento-sites
  "elcorreo.com, diariovasco.com, elnortedecastilla.es, lasprovincias.es, ideal.es, elcomercio.es, larioja.com, diariosur.es, hoy.es, laverdad.es, abc.es")

(def ^:private tools
  [{:name "front_page"
    :description "List today's articles from a Vocento newspaper front page."
    :inputSchema {:type "object"
                  :properties {:site {:type "string"
                                      :description (str "Site domain, e.g. 'diariovasco.com'. Available: " vocento-sites ". Default: elcorreo.com")}}}}
   {:name "trending"
    :description "List trending/most viewed articles from a Vocento newspaper."
    :inputSchema {:type "object"
                  :properties {:site {:type "string"
                                      :description (str "Site domain, e.g. 'diariovasco.com'. Available: " vocento-sites ". Default: elcorreo.com")}}}}
   {:name "read_comments"
    :description "Read comments on any Vocento newspaper article. Pass the full article URL."
    :inputSchema {:type "object"
                  :properties {:url {:type "string"
                                     :description "Full article URL (any Vocento site)"}}
                  :required ["url"]}}
   {:name "post_comment"
    :description "Post a comment on any Vocento newspaper article. Requires being logged in to elcorreo.com in Chrome."
    :inputSchema {:type "object"
                  :properties {:url {:type "string"
                                     :description "Full article URL (any Vocento site)"}
                               :text {:type "string"
                                      :description "Comment text"}
                               :reply_to {:type "string"
                                          :description "Comment ID to reply to (optional, omit for top-level comment)"}}
                  :required ["url" "text"]}}
   {:name "delete_comment"
    :description "Delete one of our own comments by its ID."
    :inputSchema {:type "object"
                  :properties {:comment_id {:type "string"
                                            :description "Comment ID to delete (e.g. 6331972cdeab)"}
                               :url {:type "string"
                                     :description "Article URL the comment was posted on (used to derive site)"}}
                  :required ["comment_id"]}}])

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
     :serverInfo {:name "elcorreo-mcp" :version "0.1.0"}
     :instructions "MCP server for elcorreo.com comments. Uses Chrome cookies for authentication - make sure you're logged in to elcorreo.com in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- url->stream-id [url]
  (or (api/get-stream-id url)
      (throw (ex-info (str "Could not find stream ID for article: " url) {}))))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "front_page"
            (fmt/format-front-page (api/front-page :site (or (:site arguments) "elcorreo.com")))

            "trending"
            (fmt/format-trending (api/trending :site (or (:site arguments) "elcorreo.com")))

            "read_comments"
            (let [url (:url arguments)
                  stream-id (url->stream-id url)
                  data (api/list-comments stream-id url)]
              (fmt/format-comments data))

            "post_comment"
            (let [url (:url arguments)
                  stream-id (url->stream-id url)
                  parent (or (:reply_to arguments) "root")
                  result (api/post-comment stream-id url (:text arguments) :parent-id parent)]
              (fmt/format-post-result result))

            "delete_comment"
            (let [result (api/delete-comment (:comment_id arguments) (:url arguments))]
              (if (or (nil? result) (:success result) (not (:error result)))
                (str "Comment " (:comment_id arguments) " deleted.")
                (str "Delete failed: " (pr-str result))))

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
