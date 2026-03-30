(ns meneame.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [meneame.api :as api]
            [meneame.format :as fmt])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "list_portada"
    :description "List published stories from Menéame's front page (portada)."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories (default 25, max 100)"}
                               :sub {:type "string"
                                     :description "Sub-community filter (e.g. 'actualidad', 'tecnología')"}}}}
   {:name "list_popular"
    :description "List popular stories on Menéame (most voted in last 36h)."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories (default 25, max 100)"}}}}
   {:name "list_queue"
    :description "List pending stories in the queue (cola/pendientes)."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories (default 25, max 100)"}}}}
   {:name "search"
    :description "Search stories on Menéame."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "Search query"}
                               :n {:type "number"
                                   :description "Number of results (default 25, max 100)"}}
                  :required ["query"]}}
   {:name "read_comments"
    :description "Read comments on a Menéame story. Pass the numeric link ID."
    :inputSchema {:type "object"
                  :properties {:link_id {:type "string"
                                         :description "The numeric link/story ID"}}
                  :required ["link_id"]}}
   {:name "post_comment"
    :description "Post a comment on a Menéame story. Requires being logged in to meneame.net in Chrome."
    :inputSchema {:type "object"
                  :properties {:link_id {:type "string"
                                         :description "The numeric link/story ID"}
                               :text {:type "string"
                                      :description "Comment text"}
                               :parent_id {:type "string"
                                           :description "Parent comment ID for replies (optional)"}}
                  :required ["link_id" "text"]}}])

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
     :serverInfo {:name "meneame-mcp" :version "0.1.0"}
     :instructions "MCP server for Menéame.net. Uses Chrome cookies for authentication - make sure you're logged in to meneame.net in Chrome for posting."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- clamp-n [args]
  (let [raw (:n args)
        n (cond
            (nil? raw) 25
            (number? raw) (int raw)
            (string? raw) (or (parse-long raw) 25)
            :else 25)]
    (min (max n 1) 100)))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "list_portada"
            (let [data (api/list-stories :status "published" :rows (clamp-n arguments)
                         :sub (:sub arguments))]
              (fmt/format-stories data "Portada"))

            "list_popular"
            (let [data (api/list-stories :popular true :rows (clamp-n arguments))]
              (fmt/format-stories data "Popular"))

            "list_queue"
            (let [data (api/list-stories :status "queued" :rows (clamp-n arguments))]
              (fmt/format-stories data "Cola"))

            "search"
            (let [data (api/list-stories :q (:query arguments) :rows (clamp-n arguments))]
              (fmt/format-stories data (str "Search: " (:query arguments))))

            "read_comments"
            (let [data (api/get-comments (:link_id arguments))]
              (fmt/format-comments data (:link_id arguments)))

            "post_comment"
            (api/post-comment (:link_id arguments) (:text arguments)
                              :parent-id (:parent_id arguments))

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
