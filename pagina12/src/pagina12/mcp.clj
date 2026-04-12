(ns pagina12.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader StringWriter PrintWriter]
           [java.util Base64]
           [java.net URLDecoder])
  (:gen-class))

;;; ---- Coral Talk GraphQL + Página 12 API ----

(def ^:private talk-url "https://talk.pagina12.com.ar/api/v1/graph/ql")
(def ^:private base-url "https://www.pagina12.com.ar")
(def ^:private ua "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

(defn- cookie-script
  [domain body]
  (str
   "import browser_cookie3\n"
   "orig_get_password = browser_cookie3._LinuxPasswordManager.get_password\n"
   "def patched_get_password(self, os_crypt_name):\n"
   "    try:\n"
   "        return orig_get_password(self, os_crypt_name)\n"
   "    except Exception:\n"
   "        return browser_cookie3.CHROMIUM_DEFAULT_PASSWORD\n"
   "browser_cookie3._LinuxPasswordManager.get_password = patched_get_password\n"
   "cj = browser_cookie3.chrome(domain_name=" (pr-str domain) ")\n"
   body))

(defn- get-token
  "Extract the Coral Talk JWT from the socies-auth-token Chrome cookie."
  []
  (let [script (cookie-script
                "pagina12.com.ar"
                (str "tok = next((c.value for c in cj if c.name == 'socies-auth-token'), None)\n"
                     "print(tok or '')"))
        pb (ProcessBuilder. ["python3" "-c" script])
        proc (.start pb)
        raw (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq raw)
      (let [decoded (URLDecoder/decode raw "UTF-8")
            ;; Strip Express.js s: prefix then take first 3 JWT parts
            stripped (if (str/starts-with? decoded "s:") (subs decoded 2) decoded)]
        (str/join "." (take 3 (str/split stripped #"\.")))))))

(defn- token-sub
  "Extract the Coral user ID from the JWT subject."
  [token]
  (when (seq token)
    (let [[_ payload] (str/split token #"\.")
          padded (str payload (apply str (repeat (mod (- 4 (mod (count payload) 4)) 4) "=")))
          decoded (.decode (Base64/getUrlDecoder) ^String padded)
          claims (json/read-str (String. decoded "UTF-8") :key-fn keyword)]
      (:sub claims))))

(defn- curl-json [& args]
  (let [pb (ProcessBuilder. ^java.util.List (vec args))
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- graphql
  ([query] (graphql query nil false))
  ([query variables auth?]
   (let [body (cond-> {:query query} variables (assoc :variables variables))
         token (when auth? (get-token))
         args (cond-> ["curl" "-sS" "--max-time" "20"
                       "-X" "POST"
                       "-H" "Content-Type: application/json"
                       "-H" (str "User-Agent: " ua)]
                (seq token)
                (into ["-H" (str "Authorization: Bearer " token)])
                true
                (into ["-d" (json/write-str body) talk-url]))]
     (apply curl-json args))))

;;; ---- Tools ----

(defn- api-front-page []
  (let [pb (ProcessBuilder. ["curl" "-sS" "--max-time" "15" "-A" ua base-url])
        proc (.start pb)
        html (slurp (.getInputStream proc))
        _ (.waitFor proc)
        url-pat (java.util.regex.Pattern/compile
                  "href=\"(/\\d{4}/\\d{2}/\\d{2}/[^\"'#?]+/)\"")
        url-m (.matcher url-pat html)
        title-pat (java.util.regex.Pattern/compile
                    "<[a-zA-Z][^>]*class=\"[^\"]*title[^\"]*\"[^>]*>(.*?)</[a-zA-Z]+>"
                    (bit-or java.util.regex.Pattern/CASE_INSENSITIVE
                            java.util.regex.Pattern/DOTALL))
        title-m (.matcher title-pat html)
        urls (loop [acc [] seen #{}]
               (if (.find url-m)
                 (let [u (str base-url (.group url-m 1))]
                   (if (contains? seen u)
                     (recur acc seen)
                     (recur (conj acc u) (conj seen u))))
                 acc))
        titles (loop [acc []]
                 (if (.find title-m)
                   (let [raw (.group title-m 1)
                         clean (-> raw
                                   (str/replace #"<[^>]+>" "")
                                   (str/replace #"\s+" " ")
                                   str/trim)]
                     (if (< 10 (count clean) 300)
                       (recur (conj acc clean))
                       (recur acc)))
                   acc))]
    (map-indexed (fn [i url]
                   {:url url
                    :title (or (nth titles i nil)
                               (-> url (str/split #"/") last))})
                 urls)))

(defn- article-url->asset [article-url n]
  (let [q (str "{ asset(url: \"" article-url "\") {"
               "  id title totalCommentCount"
               "  comments(query: {limit: " n ", sortOrder: DESC}) {"
               "    nodes { id body user { username } created_at"
               "      replies { nodes { id body user { username } created_at } }"
               "    }"
               "  }"
               "} }")
        resp (graphql q)]
    (-> resp :data :asset)))

(defn- api-read-comments [article-url n]
  (let [asset (article-url->asset article-url n)]
    (when-not asset
      (throw (ex-info (str "Article not found in Coral Talk: " article-url) {})))
    (let [comments (-> asset :comments :nodes)]
      (str "# " (:title asset) "\n"
           (:totalCommentCount asset) " comments total\n"
           article-url "\n\n"
           (str/join "\n\n"
             (map (fn [c]
                    (let [replies (-> c :replies :nodes)]
                      (str "**" (-> c :user :username) "** [" (:id c) "] "
                           "(" (:created_at c) ")\n"
                           (:body c)
                           (when (seq replies)
                             (str "\n" (str/join "\n"
                                         (map (fn [r]
                                                (str "  > **" (-> r :user :username)
                                                     "** [" (:id r) "]: " (:body r)))
                                              replies)))))))
                  comments))))))

(defn- api-post-comment [article-url body parent-id]
  (let [token (get-token)]
    (when-not (seq token)
      (throw (ex-info "Not logged in to pagina12.com.ar in Chrome." {})))
    (let [asset (article-url->asset article-url 1)]
      (when-not asset
        (throw (ex-info (str "Article not found: " article-url) {})))
      (let [asset-id (:id asset)
            vars (cond-> {"asset_id" asset-id "body" body}
                   parent-id (assoc "parent_id" parent-id))
            query "mutation CreateComment($asset_id: ID!, $body: String!, $parent_id: ID) {
                     createComment(input: {asset_id: $asset_id, body: $body, richTextBody: $body, parent_id: $parent_id}) {
                       comment { id body }
                       errors { translation_key }
                     }
                   }"
            resp (graphql query vars true)
            result (-> resp :data :createComment)]
        (if-let [c (:comment result)]
          (str "Comment posted [" (:id c) "]:\n" (:body c))
          (throw (ex-info (str "Post failed: " (:errors result)) {:resp resp})))))))

(defn- api-my-comments [n]
  (let [token (get-token)
        author-id (token-sub token)]
    (when-not (seq token)
      (throw (ex-info "Not logged in to pagina12.com.ar in Chrome." {})))
    (when-not (seq author-id)
      (throw (ex-info "Could not resolve current user ID from auth token." {})))
    (let [query (str "{ comments(query: { author_id: \"" author-id "\", limit: " n ", sortOrder: DESC }) {"
                      "  hasNextPage endCursor"
                      "  nodes {"
                      "    id body created_at status"
                      "    parent { id }"
                      "    asset { id title url }"
                      "    replies { nodes { id body created_at user { username } } }"
                      "  }"
                      "} }")
          resp (graphql query nil true)
          comments (get-in resp [:data :comments :nodes])]
      (if (seq comments)
        (str "# Página 12 - My comments (" (count comments) " shown)\n\n"
             (str/join "\n\n"
                       (map (fn [c]
                              (let [asset (:asset c)
                                    replies (get-in c [:replies :nodes])]
                                (str
                                 (if (:parent c) "Reply" "Comment")
                                 " [" (:id c) "]"
                                 " (" (:created_at c) ")\n"
                                 (when-let [title (:title asset)]
                                   (str title "\n"))
                                 (when-let [url (:url asset)]
                                   (str url "\n"))
                                 (:body c)
                                 (when (seq replies)
                                   (str "\nReplies:\n"
                                        (str/join "\n"
                                                  (map (fn [r]
                                                         (str "- " (or (get-in r [:user :username]) "unknown")
                                                              " [" (:created_at r) "]: "
                                                              (:body r)))
                                                          replies)))))))
                            comments)))
        "No comments found for the current user."))))

(defn- api-my-replies [n]
  (let [token (get-token)
        author-id (token-sub token)]
    (when-not (seq token)
      (throw (ex-info "Not logged in to pagina12.com.ar in Chrome." {})))
    (when-not (seq author-id)
      (throw (ex-info "Could not resolve current user ID from auth token." {})))
    (let [query (str "{ comments(query: { author_id: \"" author-id "\", limit: 100, sortOrder: DESC }) {"
                      "  nodes {"
                      "    id body created_at"
                      "    asset { title url }"
                      "    replies { nodes { id body created_at user { username } } }"
                      "  }"
                      "} }")
          resp (graphql query nil true)
          comments (->> (get-in resp [:data :comments :nodes])
                        (filter #(seq (get-in % [:replies :nodes])))
                        (take n))]
      (if (seq comments)
        (str "# Página 12 - Replies to my comments (" (count comments) " threads)\n\n"
             (str/join "\n\n"
                       (map (fn [c]
                              (let [asset (:asset c)
                                    replies (get-in c [:replies :nodes])]
                                (str "Your comment [" (:id c) "]"
                                     " (" (:created_at c) ")\n"
                                     (when-let [title (:title asset)]
                                       (str title "\n"))
                                     (when-let [url (:url asset)]
                                       (str url "\n"))
                                     (:body c)
                                     "\nReplies:\n"
                                     (str/join "\n"
                                               (map (fn [r]
                                                      (str "- " (or (get-in r [:user :username]) "unknown")
                                                           " [" (:created_at r) "]: "
                                                           (:body r)))
                                                    replies)))))
                            comments)))
        "No direct replies found on your recent comments."))))

;;; ---- MCP boilerplate ----

(def ^:private tools
  [{:name "front_page"
    :description "List today's articles from Página 12 (Argentine progressive newspaper). Returns titles and URLs."
    :inputSchema {:type "object" :properties {}}}
   {:name "read_comments"
    :description "Read comments on a Página 12 article via Coral Talk. Returns comment text, usernames, and IDs (needed for replies)."
    :inputSchema {:type "object"
                  :properties {:article_url {:type "string"
                                             :description "Full URL of the Página 12 article"}
                               :n {:type "number"
                                   :description "Number of comments to fetch (default 30, max 100)"}}
                  :required ["article_url"]}}
   {:name "post_comment"
    :description "Post a comment on a Página 12 article. Uses Chrome cookies for auth - must be logged in to pagina12.com.ar in Chrome."
    :inputSchema {:type "object"
                  :properties {:article_url {:type "string"
                                             :description "Full URL of the Página 12 article to comment on"}
                               :body {:type "string"
                                      :description "Comment text"}
                               :parent_id {:type "string"
                                           :description "ID of comment to reply to (optional, for threaded replies)"}}
                  :required ["article_url" "body"]}}
   {:name "my_comments"
    :description "List the current logged-in user's recent comments on Página 12, including article links and direct replies when available."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of comments to fetch (default 20, max 100)"}}
                  :required []}}
   {:name "my_replies"
    :description "List only the current logged-in user's recent comment threads that already have direct replies."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of replied threads to fetch (default 20, max 100)"}}
                  :required []}}])

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
     :serverInfo {:name "pagina12-mcp" :version "0.1.0"}
     :instructions "MCP server for Página 12 (www.pagina12.com.ar), Argentina's main progressive newspaper. Uses Chrome cookies for authentication - make sure you're logged in to pagina12.com.ar in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- clamp [n default mx]
  (min (max (or (when n (Long/parseLong (str n))) default) 1) mx))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "front_page"
            (let [articles (api-front-page)]
              (str "# Página 12 - Front page (" (count articles) " articles)\n\n"
                   (str/join "\n" (map-indexed (fn [i {:keys [title url]}]
                                                 (str (inc i) ". " title "\n   " url))
                                               articles))))

            "read_comments"
            (api-read-comments (:article_url arguments)
                               (clamp (:n arguments) 30 100))

            "post_comment"
            (api-post-comment (:article_url arguments)
                              (:body arguments)
                              (:parent_id arguments))

            "my_comments"
            (api-my-comments (clamp (:n arguments) 20 100))

            "my_replies"
            (api-my-replies (clamp (:n arguments) 20 100))

            (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (let [sw (StringWriter.)
            pw (PrintWriter. sw)]
        (.printStackTrace e pw)
        (respond id (tool-result (str "Error: " (.getMessage e) "\n\n" (.toString sw))
                                 :error? true))))))

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
