(ns bluesky.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [bluesky.api :as api]
            [bluesky.format :as fmt])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "timeline"
    :description "Get your Bluesky home timeline."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of posts (default 25, max 100)"}}}}
   {:name "search"
    :description "Search Bluesky posts."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "Search query"}
                               :n {:type "number"
                                   :description "Number of results (default 25, max 100)"}
                               :sort {:type "string"
                                      :description "Sort order: 'latest' or 'top' (default latest)"}}
                  :required ["query"]}}
   {:name "read_thread"
    :description "Read a Bluesky post and its replies."
    :inputSchema {:type "object"
                  :properties {:uri {:type "string"
                                     :description "Post URL (https://bsky.app/profile/.../post/...) or AT URI (at://did/.../rkey)"}}
                  :required ["uri"]}}
   {:name "notifications"
    :description "Get your Bluesky notifications (replies, mentions, likes, follows)."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of notifications (default 25, max 50)"}}}}
   {:name "post"
    :description "Create a new Bluesky post. Max 300 characters."
    :inputSchema {:type "object"
                  :properties {:text {:type "string"
                                      :description "Post text (max 300 chars)"}}
                  :required ["text"]}}
   {:name "follow"
    :description "Follow a Bluesky user by their handle."
    :inputSchema {:type "object"
                  :properties {:handle {:type "string"
                                        :description "Bluesky handle (e.g. user.bsky.social, with or without @)"}}
                  :required ["handle"]}}
   {:name "reply"
    :description "Reply to a Bluesky post or comment."
    :inputSchema {:type "object"
                  :properties {:uri {:type "string"
                                     :description "AT URI or bsky.app URL of the post to reply to"}
                               :text {:type "string"
                                      :description "Reply text (max 300 chars)"}}
                  :required ["uri" "text"]}}])

(defn- respond [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn- error-response [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn- tool-result [text & {:keys [error?]}]
  {:content [{:type "text" :text text}]
   :isError (boolean error?)})

(defn- clamp-n [args default]
  (let [raw (:n args)
        n (cond
            (nil? raw) default
            (number? raw) (int raw)
            (string? raw) (or (parse-long raw) default)
            :else default)]
    (min (max n 1) 100)))

(defn- handle-initialize [id _params]
  (respond id
    {:protocolVersion "2024-11-05"
     :capabilities {:tools {}}
     :serverInfo {:name "bluesky-mcp" :version "0.1.0"}
     :instructions "MCP server for Bluesky. Read-only tools work without auth via the public AppView API. For authenticated actions, set BLUESKY_HANDLE and BLUESKY_APP_PASSWORD or create ~/.config/social-mcp/bluesky.edn with :handle and :app-password."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "timeline"
            (fmt/format-feed (api/timeline :limit (clamp-n arguments 25))
                             (if (api/read-only-mode?) "Discover" "Timeline"))

            "search"
            (fmt/format-feed (api/search (:query arguments)
                                         :limit (clamp-n arguments 25)
                                         :sort (or (:sort arguments) "latest"))
                             (str "Search: " (:query arguments)))

            "read_thread"
            (fmt/format-thread (api/get-thread (:uri arguments)))

            "notifications"
            (fmt/format-notifications (api/notifications :limit (min (clamp-n arguments 25) 50)))

            "post"
            (let [resp (api/post (:text arguments))]
              (if (:uri resp)
                (str "Posted successfully.\nURI: " (:uri resp))
                (str "Unexpected response: " (pr-str resp))))

            "follow"
            (api/follow (:handle arguments))

            "reply"
            (let [resp (api/reply (:uri arguments) (:text arguments))]
              (if (:uri resp)
                (str "Reply posted successfully.\nURI: " (:uri resp))
                (str "Unexpected response: " (pr-str resp))))

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
