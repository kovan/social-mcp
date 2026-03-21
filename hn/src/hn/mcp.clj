(ns hn.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hn.forum :as forum])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "list_top_stories"
    :description "List the current top stories on Hacker News."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories to return (default 30, max 100)"}}}}
   {:name "list_new_stories"
    :description "List the newest stories on Hacker News."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories to return (default 30, max 100)"}}}}
   {:name "list_best_stories"
    :description "List the best stories on Hacker News."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories to return (default 30, max 100)"}}}}
   {:name "list_ask_stories"
    :description "List Ask HN stories."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories to return (default 30, max 100)"}}}}
   {:name "list_show_stories"
    :description "List Show HN stories."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories to return (default 30, max 100)"}}}}
   {:name "read_thread"
    :description "Read a Hacker News story and its comment thread. Pass the numeric item ID."
    :inputSchema {:type "object"
                  :properties {:item_id {:type "string"
                                         :description "The numeric item ID (e.g. 12345)"}}
                  :required ["item_id"]}}
   {:name "reply_comment"
    :description "Reply to a comment or story on Hacker News. Requires being logged in to HN in Chrome."
    :inputSchema {:type "object"
                  :properties {:item_id {:type "string"
                                         :description "The numeric item ID to reply to"}
                               :message {:type "string"
                                         :description "Reply text (plain text, blank lines for paragraphs)"}}
                  :required ["item_id" "message"]}}
   {:name "submit_story"
    :description "Submit a new story to Hacker News. Provide title and either url or text. Requires being logged in to HN in Chrome."
    :inputSchema {:type "object"
                  :properties {:title {:type "string"
                                       :description "Story title"}
                               :url {:type "string"
                                     :description "URL to submit (mutually exclusive with text)"}
                               :text {:type "string"
                                      :description "Text for Ask HN / text posts (mutually exclusive with url)"}}
                  :required ["title"]}}
   {:name "list_replies"
    :description "List replies to your comments on Hacker News. Shows recent responses from other users."
    :inputSchema {:type "object"
                  :properties {}}}])

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
     :serverInfo {:name "hn-mcp" :version "0.1.0"}
     :instructions "MCP server for Hacker News. Uses Chrome cookies for authentication - make sure you're logged in to news.ycombinator.com in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- clamp-n [args]
  (let [raw (:n args)
        n (cond
            (nil? raw) 30
            (number? raw) (int raw)
            (string? raw) (or (parse-long raw) 30)
            :else 30)]
    (min (max n 1) 100)))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result (case name
                   "list_top_stories"  (forum/list-stories :top (clamp-n arguments))
                   "list_new_stories"  (forum/list-stories :new (clamp-n arguments))
                   "list_best_stories" (forum/list-stories :best (clamp-n arguments))
                   "list_ask_stories"  (forum/list-stories :ask (clamp-n arguments))
                   "list_show_stories" (forum/list-stories :show (clamp-n arguments))
                   "read_thread"       (forum/read-thread (:item_id arguments))
                   "reply_comment"     (forum/reply-comment (:item_id arguments) (:message arguments))
                   "submit_story"      (forum/submit-story (:title arguments)
                                         :url (:url arguments)
                                         :text (:text arguments))
                   "list_replies"      (forum/list-replies)
                   (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (let [sw (java.io.StringWriter.)
            pw (java.io.PrintWriter. sw)]
        (.printStackTrace e pw)
        (respond id (tool-result (str "Error: " (.getMessage e) "\n\n" (.toString sw)) :error? true))))))

(defn- handle-message [msg]
  (let [{:keys [id method params]} msg]
    (case method
      "initialize"                (handle-initialize id params)
      "notifications/initialized" nil
      "tools/list"                (handle-tools-list id params)
      "tools/call"                (handle-tools-call id params)
      "ping"                      (respond id {})
      ;; unknown
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
