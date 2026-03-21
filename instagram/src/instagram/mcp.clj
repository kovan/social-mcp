(ns instagram.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [instagram.api :as api])
  (:import [java.io BufferedReader InputStreamReader StringWriter PrintWriter])
  (:gen-class))

(def ^:private tools
  [{:name "hashtag_top"
    :description "Get top posts for an Instagram hashtag."
    :inputSchema {:type "object"
                  :properties {:hashtag {:type "string"
                                         :description "Hashtag name (with or without #)"}
                               :n {:type "number"
                                   :description "Number of posts to return (default 9, max 50)"}}
                  :required ["hashtag"]}}
   {:name "hashtag_recent"
    :description "Get recent posts for an Instagram hashtag."
    :inputSchema {:type "object"
                  :properties {:hashtag {:type "string"
                                         :description "Hashtag name (with or without #)"}
                               :n {:type "number"
                                   :description "Number of posts to return (default 20, max 50)"}}
                  :required ["hashtag"]}}
   {:name "post_info"
    :description "Get details and top comments for an Instagram post."
    :inputSchema {:type "object"
                  :properties {:url {:type "string"
                                     :description "Instagram post URL (e.g. https://www.instagram.com/p/ABC123/) or shortcode"}}
                  :required ["url"]}}
   {:name "post_comment"
    :description "Post a comment on an Instagram post. Requires INSTAGRAM_USERNAME and INSTAGRAM_PASSWORD env vars."
    :inputSchema {:type "object"
                  :properties {:url {:type "string"
                                     :description "Instagram post URL or shortcode"}
                               :text {:type "string"
                                      :description "Comment text"}}
                  :required ["url" "text"]}}
   {:name "user_posts"
    :description "Get recent posts from an Instagram user."
    :inputSchema {:type "object"
                  :properties {:username {:type "string"
                                          :description "Instagram username (with or without @)"}
                               :n {:type "number"
                                   :description "Number of posts to return (default 12, max 50)"}}
                  :required ["username"]}}
   {:name "user_info"
    :description "Get profile information for an Instagram user."
    :inputSchema {:type "object"
                  :properties {:username {:type "string"
                                          :description "Instagram username (with or without @)"}}
                  :required ["username"]}}
   {:name "search_hashtags"
    :description "Search Instagram hashtags by keyword."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "Search query"}}
                  :required ["query"]}}
   {:name "account_new_posts"
    :description "Get recent posts from an account that haven't been commented on yet. Tracks state in ~/.instagram_state.json."
    :inputSchema {:type "object"
                  :properties {:username {:type "string"
                                          :description "Instagram username"}
                               :n {:type "number"
                                   :description "How many recent posts to check (default 20)"}}
                  :required ["username"]}}
   {:name "mark_commented"
    :description "Mark a post as already commented on so it won't be returned by account_new_posts again."
    :inputSchema {:type "object"
                  :properties {:shortcode {:type "string"
                                           :description "Post shortcode (the part after /p/ in the URL)"}}
                  :required ["shortcode"]}}
   {:name "follow_user"
    :description "Follow an Instagram user."
    :inputSchema {:type "object"
                  :properties {:username {:type "string"
                                          :description "Instagram username (with or without @)"}}
                  :required ["username"]}}
   {:name "generate_image"
    :description "Generate an Instagram image (stat card or opinion card) and return the file path. Then use upload_photo to post it."
    :inputSchema {:type "object"
                  :properties {:kind {:type "string"
                                      :description "\"stat\" for data card, \"opinion\" for analysis card"}
                               :palette {:type "string"
                                         :description "Color theme: \"dark\" (blue), \"red\", \"green\", \"gold\""}
                               :stat {:type "string"
                                      :description "[stat cards] The big number/value (e.g. \"2,4%\")"}
                               :label {:type "string"
                                       :description "[stat cards] Short label below the stat (e.g. \"Inflación mensual\")"}
                               :context {:type "string"
                                         :description "[stat cards] One or two sentences of context"}
                               :headline {:type "string"
                                          :description "[opinion cards] Bold headline (1 line ideally)"}
                               :body {:type "string"
                                      :description "[opinion cards] Body paragraph (2-4 sentences)"}}
                  :required ["kind"]}}
   {:name "upload_photo"
    :description "Upload a photo to Instagram with a caption."
    :inputSchema {:type "object"
                  :properties {:path {:type "string"
                                      :description "Absolute path to the image file (JPEG or PNG)"}
                               :caption {:type "string"
                                         :description "Post caption including hashtags"}}
                  :required ["path"]}}])

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
     :serverInfo {:name "instagram-mcp" :version "0.1.0"}
     :instructions "MCP server for Instagram. Requires INSTAGRAM_USERNAME and INSTAGRAM_PASSWORD environment variables."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- clamp-n [args default]
  (let [raw (:n args)
        n (cond
            (nil? raw) default
            (number? raw) (int raw)
            (string? raw) (or (parse-long raw) default)
            :else default)]
    (min (max n 1) 50)))

(defn- relative-time [iso-str]
  (when (seq iso-str)
    (try
      (let [instant (java.time.Instant/parse (str/replace iso-str " " "T"))
            now (java.time.Instant/now)
            diff (.getEpochSecond (.minus now instant java.time.temporal.ChronoUnit/SECONDS))
            diff (Math/abs diff)]
        (cond
          (< diff 60) "just now"
          (< diff 3600) (str (quot diff 60) "m ago")
          (< diff 86400) (str (quot diff 3600) "h ago")
          :else (str (quot diff 86400) "d ago")))
      (catch Exception _ iso-str))))

(defn- format-media-item [i {:keys [user caption likes comments url timestamp shortcode]}]
  (str (inc i) ". **@" (or user "?") "**"
       (when (seq (str timestamp)) (str " - " (relative-time timestamp))) "\n"
       "   " (or url (str "https://www.instagram.com/p/" shortcode "/")) "\n"
       "   Likes: " (or likes 0) " | Comments: " (or comments 0) "\n"
       (when (seq caption)
         (let [cap (str/trim caption)
               short (subs cap 0 (min 200 (count cap)))]
           (str "   " short (when (> (count cap) 200) "...") "\n")))))

(defn- format-media-list [items title]
  (if (seq items)
    (str "# " title " (" (count items) " posts)\n\n"
         (str/join "\n" (map-indexed format-media-item items)))
    (str "# " title "\n\nNo posts found.")))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "hashtag_top"
            (let [tag (:hashtag arguments)
                  items (api/hashtag-top tag (clamp-n arguments 9))]
              (format-media-list items (str "Top posts - #" (str/replace tag "#" ""))))

            "hashtag_recent"
            (let [tag (:hashtag arguments)
                  items (api/hashtag-recent tag (clamp-n arguments 20))]
              (format-media-list items (str "Recent posts - #" (str/replace tag "#" ""))))

            "post_info"
            (let [{:keys [user caption likes comments url timestamp top_comments]
                   :as post} (api/post-info (:url arguments))]
              (str "# Post by @" (or user "?") "\n"
                   (or url "") "\n"
                   (when (seq (str timestamp)) (str (relative-time timestamp) "\n"))
                   "Likes: " (or likes 0) " | Comments: " (or comments 0) "\n\n"
                   (when (seq caption) (str caption "\n\n"))
                   (when (seq top_comments)
                     (str "## Top comments\n\n"
                          (str/join "\n\n"
                            (map (fn [{:keys [user text likes timestamp]}]
                                   (str "**@" (or user "?") "**"
                                        (when (seq (str timestamp)) (str " (" (relative-time timestamp) ")"))
                                        (when (pos? (or likes 0)) (str " - " likes " likes"))
                                        "\n" text))
                                 top_comments))))))

            "post_comment"
            (let [result (api/post-comment (:url arguments) (:text arguments))]
              (str "Comment posted successfully. ID: " (:id result)))

            "user_posts"
            (let [username (:username arguments)
                  items (api/user-posts username (clamp-n arguments 12))]
              (format-media-list items (str "Posts by @" (str/replace username "@" ""))))

            "user_info"
            (let [{:keys [username full_name bio followers following posts
                           is_private is_verified]} (api/user-info (:username arguments))]
              (str "# @" username (when (seq full_name) (str " (" full_name ")"))
                   (when is_verified " [verified]")
                   (when is_private " [private]") "\n\n"
                   "Followers: " (or followers 0)
                   " | Following: " (or following 0)
                   " | Posts: " (or posts 0) "\n\n"
                   (when (seq bio) (str bio "\n"))))

            "search_hashtags"
            (let [results (api/search-hashtags (:query arguments))]
              (if (seq results)
                (str "# Hashtag search: " (:query arguments) "\n\n"
                     (str/join "\n"
                       (map (fn [{:keys [name media_count]}]
                              (str "#" name " - " (or media_count "?") " posts"))
                            results)))
                (str "No hashtags found for: " (:query arguments))))

            "account_new_posts"
            (let [username (:username arguments)
                  items (api/account-new-posts username (clamp-n arguments 20))]
              (if (seq items)
                (format-media-list items (str "New unread posts - @" (str/replace username "@" "")))
                (str "No new posts from @" (str/replace username "@" "") " since last check.")))

            "mark_commented"
            (do (api/mark-commented (:shortcode arguments))
                (str "Marked " (:shortcode arguments) " as commented."))

            "generate_image"
            (let [{:keys [path]} (api/generate-image arguments)]
              (str "Image generated: " path "\nUse upload_photo with this path to post it."))

            "follow_user"
            (let [{:keys [followed username]} (api/follow-user (:username arguments))]
              (if followed
                (str "Now following @" username)
                (str "Failed to follow @" username)))

            "upload_photo"
            (let [{:keys [url user shortcode]} (api/upload-photo (:path arguments)
                                                                  (or (:caption arguments) ""))]
              (str "Photo posted by @" user "\n" (or url (str "https://www.instagram.com/p/" shortcode "/"))))

            (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (let [sw (StringWriter.)
            pw (PrintWriter. sw)]
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
