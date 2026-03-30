(ns pikabu.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [pikabu.api :as api]
            [pikabu.format :as fmt]
            [pikabu.web :as web]
            [pikabu.proxy :as px])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "list_hot"
    :description "List hot stories from Pikabu front page."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories (default 10, max 30)"}
                               :page {:type "number"
                                      :description "Page number (default 1)"}}}}
   {:name "list_new"
    :description "List newest stories from Pikabu."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories (default 10, max 30)"}
                               :page {:type "number"
                                      :description "Page number (default 1)"}}}}
   {:name "list_best"
    :description "List best stories from Pikabu."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of stories (default 10, max 30)"}
                               :page {:type "number"
                                      :description "Page number (default 1)"}}}}
   {:name "list_community"
    :description "List stories from a Pikabu community."
    :inputSchema {:type "object"
                  :properties {:community {:type "string"
                                           :description "Community name (e.g. 'linux', 'science')"}
                               :n {:type "number"
                                   :description "Number of stories (default 10, max 30)"}
                               :page {:type "number"
                                      :description "Page number (default 1)"}}
                  :required ["community"]}}
   {:name "read_story"
    :description "Read a Pikabu story with its comments."
    :inputSchema {:type "object"
                  :properties {:story_id {:type "string"
                                          :description "Story ID (numeric, from listing output e.g. [story:12345])"}}
                  :required ["story_id"]}}
   {:name "search"
    :description "Search Pikabu stories."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "Search query"}
                               :n {:type "number"
                                   :description "Number of results (default 10, max 30)"}
                               :page {:type "number"
                                      :description "Page number (default 1)"}}
                  :required ["query"]}}
   {:name "post_comment"
    :description "Post a comment on a Pikabu story. Requires being logged in to pikabu.ru in Chrome."
    :inputSchema {:type "object"
                  :properties {:story_id {:type "string"
                                          :description "Story ID to comment on"}
                               :text {:type "string"
                                      :description "Comment text"}
                               :parent_id {:type "string"
                                           :description "Parent comment ID for replies (omit for top-level comment)"}}
                  :required ["story_id" "text"]}}
   {:name "reply_comment"
    :description "Reply to a comment on a Pikabu story. Requires being logged in to pikabu.ru in Chrome."
    :inputSchema {:type "object"
                  :properties {:story_id {:type "string"
                                          :description "Story ID containing the comment"}
                               :parent_id {:type "string"
                                           :description "Comment ID to reply to"}
                               :text {:type "string"
                                      :description "Reply text"}}
                  :required ["story_id" "parent_id" "text"]}}
   {:name "notifications"
    :description "Get replies to your comments. Single request via /answers page. Use page parameter for older replies."
    :inputSchema {:type "object"
                  :properties {:page {:type "number"
                                      :description "Page number (default 1)"}}}}
   {:name "mark_answers_read"
    :description "Clear the answers bell by marking all replies as read."
    :inputSchema {:type "object"
                  :properties {}}}
   {:name "set_proxy"
    :description "Set or clear the HTTP proxy for all Pikabu requests. Useful to bypass rate limiting (429). Pass empty string to clear."
    :inputSchema {:type "object"
                  :properties {:url {:type "string"
                                     :description "Proxy URL (e.g. http://1.2.3.4:8080) or empty to clear"}}
                  :required ["url"]}}])

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
     :serverInfo {:name "pikabu-mcp" :version "0.1.0"}
     :instructions "MCP server for Pikabu (pikabu.ru). Uses Chrome cookies for authentication - make sure you're logged in to pikabu.ru in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- clamp-n [args]
  (let [raw (:n args)
        n (cond
            (nil? raw) 10
            (number? raw) (int raw)
            (string? raw) (or (parse-long raw) 10)
            :else 10)]
    (min (max n 1) 30)))

(defn- get-page [args]
  (let [raw (:page args)]
    (cond
      (nil? raw) 1
      (number? raw) (max 1 (int raw))
      (string? raw) (max 1 (or (parse-long raw) 1))
      :else 1)))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "list_hot"
            (let [stories (api/listing nil (clamp-n arguments) :page (get-page arguments))]
              (fmt/format-listing stories "Hot stories"))

            "list_new"
            (let [stories (api/listing "new" (clamp-n arguments) :page (get-page arguments))]
              (fmt/format-listing stories "New stories"))

            "list_best"
            (let [stories (api/listing "best" (clamp-n arguments) :page (get-page arguments))]
              (fmt/format-listing stories "Best stories"))

            "list_community"
            (let [community (:community arguments)
                  stories (api/listing (str "community/" community) (clamp-n arguments)
                            :page (get-page arguments))]
              (fmt/format-listing stories (str "Community: " community)))

            "read_story"
            (let [sid (:story_id arguments)
                  story-data (api/story sid)
                  comments-data (api/comments sid)]
              (if story-data
                (fmt/format-story-full story-data comments-data)
                (str "Story " sid " not found.")))

            "search"
            (let [stories (api/search (:query arguments) (clamp-n arguments)
                            :page (get-page arguments))]
              (fmt/format-listing stories (str "Search: " (:query arguments))))

            "post_comment"
            (web/post-comment (:story_id arguments) (:text arguments))

            "reply_comment"
            (web/post-comment (:story_id arguments) (:text arguments)
              :parent-id (:parent_id arguments))

            "notifications"
            (web/notifications :page (get-page arguments))

            "mark_answers_read"
            (web/mark-answers-read)

            "set_proxy"
            (let [url (str/trim (or (:url arguments) ""))]
              (if (seq url)
                (do (px/set-proxy! url)
                    (str "Proxy set to: " url))
                (do (px/set-proxy! nil)
                    "Proxy cleared.")))

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
