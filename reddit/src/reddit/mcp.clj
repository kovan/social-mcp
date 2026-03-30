(ns reddit.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [reddit.api :as api]
            [reddit.format :as fmt]
            [reddit.web :as web])
  (:import [java.io BufferedReader InputStreamReader StringWriter PrintWriter])
  (:gen-class))

(def ^:private tools
  [{:name "list_hot"
    :description "List hot posts from a subreddit or the front page."
    :inputSchema {:type "object"
                  :properties {:subreddit {:type "string"
                                           :description "Subreddit name without r/ prefix (omit for front page)"}
                               :n {:type "number"
                                   :description "Number of posts to return (default 25, max 100)"}}}}
   {:name "list_new"
    :description "List newest posts from a subreddit."
    :inputSchema {:type "object"
                  :properties {:subreddit {:type "string"
                                           :description "Subreddit name without r/ prefix (omit for front page)"}
                               :n {:type "number"
                                   :description "Number of posts to return (default 25, max 100)"}}}}
   {:name "list_top"
    :description "List top posts from a subreddit."
    :inputSchema {:type "object"
                  :properties {:subreddit {:type "string"
                                           :description "Subreddit name without r/ prefix"}
                               :n {:type "number"
                                   :description "Number of posts to return (default 25, max 100)"}
                               :time {:type "string"
                                      :description "Time period: hour, day, week, month, year, all (default day)"}}}}
   {:name "read_thread"
    :description "Read a Reddit post and its comment thread. Pass either a full Reddit URL or subreddit + post_id."
    :inputSchema {:type "object"
                  :properties {:url {:type "string"
                                     :description "Full Reddit post URL (e.g. https://www.reddit.com/r/argentina/comments/1abc23/...)"}
                               :subreddit {:type "string"
                                           :description "Subreddit name without r/ prefix (if not using url)"}
                               :post_id {:type "string"
                                         :description "Post ID from the URL (e.g. '1abc23') (if not using url)"}}}}
   {:name "search"
    :description "Search Reddit posts."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "Search query"}
                               :subreddit {:type "string"
                                           :description "Limit search to this subreddit (optional)"}
                               :sort {:type "string"
                                      :description "Sort by: relevance, hot, top, new, comments (default relevance)"}
                               :time {:type "string"
                                      :description "Time period: hour, day, week, month, year, all (default all)"}
                               :n {:type "number"
                                   :description "Number of results (default 25, max 100)"}}
                  :required ["query"]}}
   {:name "reply_comment"
    :description "Reply to a comment or post on Reddit. Requires being logged in to Reddit in Chrome."
    :inputSchema {:type "object"
                  :properties {:thing_id {:type "string"
                                          :description "The fullname of the thing to reply to (e.g. t3_abc123 for posts, t1_def456 for comments)"}
                               :message {:type "string"
                                         :description "Reply text (Reddit markdown supported)"}}
                  :required ["thing_id" "message"]}}
   {:name "submit_post"
    :description "Submit a new post to a subreddit. Requires being logged in to Reddit in Chrome."
    :inputSchema {:type "object"
                  :properties {:subreddit {:type "string"
                                           :description "Subreddit name without r/ prefix"}
                               :title {:type "string"
                                       :description "Post title"}
                               :kind {:type "string"
                                      :description "Post type: 'self' for text posts, 'link' for link posts"}
                               :text {:type "string"
                                      :description "Post body text (for self posts)"}
                               :url {:type "string"
                                     :description "URL to submit (for link posts)"}
                               :flair_id {:type "string"
                                          :description "Flair ID (use list_flairs to get available IDs)"}}
                  :required ["subreddit" "title" "kind"]}}
   {:name "list_flairs"
    :description "List available post flairs for a subreddit. Returns flair names and IDs."
    :inputSchema {:type "object"
                  :properties {:subreddit {:type "string"
                                           :description "Subreddit name without r/ prefix"}}
                  :required ["subreddit"]}}
   {:name "inbox"
    :description "Check your Reddit inbox for replies and messages."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of messages to return (default 25, max 100)"}}}}
   {:name "mark_inbox_read"
    :description "Mark Reddit inbox messages as read. Pass specific message IDs or leave empty to mark all as read."
    :inputSchema {:type "object"
                  :properties {:ids {:type "string"
                                     :description "Comma-separated fullnames to mark read (e.g. 't1_abc,t4_def'). Omit to mark all as read."}}}}
   {:name "my_comments"
    :description "List your own recent Reddit comments. Useful to check if you've already replied to a thread before replying again."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of comments to return (default 25, max 100)"}}}}])

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
     :serverInfo {:name "reddit-mcp" :version "0.1.0"}
     :instructions "MCP server for Reddit. Uses Chrome cookies for authentication - make sure you're logged in to reddit.com in Chrome."}))

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
            "list_hot"
            (let [data (api/listing (:subreddit arguments) :hot (clamp-n arguments))
                  sub (or (:subreddit arguments) "front page")]
              (fmt/format-listing data (str "Hot posts - " sub)))

            "list_new"
            (let [data (api/listing (:subreddit arguments) :new (clamp-n arguments))
                  sub (or (:subreddit arguments) "front page")]
              (fmt/format-listing data (str "New posts - " sub)))

            "list_top"
            (let [data (api/listing (:subreddit arguments) :top (clamp-n arguments)
                         :time-period (or (:time arguments) "day"))
                  sub (or (:subreddit arguments) "front page")]
              (fmt/format-listing data (str "Top posts - " sub)))

            "read_thread"
            (let [url-str (:url arguments)
                  [subreddit post-id]
                  (if (seq url-str)
                    (let [m (re-find #"/r/([^/]+)/comments/([^/?#]+)" url-str)]
                      (when-not m
                        (throw (ex-info "Cannot parse subreddit/post_id from URL" {:url url-str})))
                      [(second m) (nth m 2)])
                    [(:subreddit arguments) (:post_id arguments)])
                  _ (when-not (and (seq subreddit) (seq post-id))
                      (throw (ex-info "Provide either url or subreddit+post_id" {})))
                  data (web/read-thread subreddit post-id)]
              (fmt/format-thread data))

            "search"
            (let [data (api/search (:query arguments) (clamp-n arguments)
                         :subreddit (:subreddit arguments)
                         :sort (:sort arguments)
                         :time-period (:time arguments))]
              (fmt/format-listing data (str "Search: " (:query arguments))))

            "reply_comment"
            (web/reply-comment (:thing_id arguments) (:message arguments))

            "submit_post"
            (web/submit-post (:subreddit arguments) (:title arguments)
                             (:kind arguments)
                             :text (:text arguments)
                             :url (:url arguments)
                             :flair-id (:flair_id arguments))

            "list_flairs"
            (web/list-flairs (:subreddit arguments))

            "inbox"
            (web/inbox (clamp-n arguments))

            "mark_inbox_read"
            (web/mark-inbox-read (:ids arguments))

            "my_comments"
            (web/my-comments (clamp-n arguments))

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
