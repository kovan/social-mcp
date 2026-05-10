(ns netflix.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [netflix.web :as web])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def tools
  [{:name "search_netflix"
    :description "Search Netflix with the logged-in Chrome session and return visible title cards."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"}
                               :max_results {:type "number" :default 10}
                               :profile_name {:type "string" :default "Javier"}}
                  :required ["query"]}}
   {:name "open_netflix"
    :description "Open Netflix in the controlled Chrome profile, useful for logging in once before using search/add tools."
    :inputSchema {:type "object"
                  :properties {:url {:type "string"
                                      :default "https://www.netflix.com/login"}}}}
   {:name "list_netflix_tags"
    :description "List built-in Netflix tag presets. Presets map friendly tags to a search query or Netflix genre code."
    :inputSchema {:type "object"
                  :properties {}}}
   {:name "search_netflix_tag"
    :description "Search Netflix by a preset tag, a free-form query, or a Netflix genre code. Tags are convenience presets, not official Netflix tags."
    :inputSchema {:type "object"
                  :properties {:tag {:type "string"
                                     :description "Preset tag name, for example korean_romance, indian_movies, feel_good, comedies."}
                               :query {:type "string"
                                       :description "Free-form Netflix search query. Used when tag/code is not supplied."}
                               :code {:type "string"
                                      :description "Netflix genre code, used with /browse/genre/{code}."}
                               :max_results {:type "number" :default 10}
                               :profile_name {:type "string" :default "Javier"}}}}
   {:name "browse_netflix_genre"
    :description "Browse Netflix by a numeric genre/category code using /browse/genre/{code}."
    :inputSchema {:type "object"
                  :properties {:code {:type "string"}
                               :max_results {:type "number" :default 10}
                               :profile_name {:type "string" :default "Javier"}}
                  :required ["code"]}}
   {:name "add_to_my_list"
    :description "Search a Netflix title and click its Add to My List button. Set dry_run to true to only verify the match."
    :inputSchema {:type "object"
                  :properties {:title {:type "string"}
                               :dry_run {:type "boolean" :default false}
                               :profile_name {:type "string" :default "Javier"}}
                  :required ["title"]}}
   {:name "bulk_add_to_my_list"
    :description "Add several Netflix titles to My List, one by one. Input titles is an array of strings."
    :inputSchema {:type "object"
                  :properties {:titles {:type "array"
                                        :items {:type "string"}}
                               :dry_run {:type "boolean" :default false}
                               :profile_name {:type "string" :default "Javier"}}
                  :required ["titles"]}}])

(defn- respond [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn- error-response [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn- tool-result [text & {:keys [error?]}]
  {:content [{:type "text" :text text}]
   :isError (boolean error?)})

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result (case name
                   "search_netflix"
                   (web/search-titles (:query arguments) arguments)

                   "open_netflix"
                   (web/open-netflix arguments)

                   "list_netflix_tags"
                   (web/list-tag-presets)

                   "search_netflix_tag"
                   (web/search-tag arguments)

                   "browse_netflix_genre"
                   (web/browse-genre (:code arguments) arguments)

                   "add_to_my_list"
                   (web/add-to-my-list (:title arguments) arguments)

                   "bulk_add_to_my_list"
                   (web/bulk-add-to-my-list arguments)

                   (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result (json/write-str result))))
    (catch Exception e
      (respond id (tool-result (str "Error: " (.getMessage e)) :error? true)))))

(defn- handle-message [{:keys [id method params]}]
  (case method
    "initialize" (respond id {:protocolVersion "2024-11-05"
                              :capabilities {:tools {}}
                              :serverInfo {:name "netflix-mcp" :version "0.1.0"}
                              :instructions "Netflix MCP. Uses Chrome cookies and CDP-controlled Chrome to search titles and add them to My List. Log in to netflix.com in Chrome first."})
    "notifications/initialized" nil
    "tools/list" (respond id {:tools tools})
    "tools/call" (handle-tools-call id params)
    "ping" (respond id {})
    (when id (error-response id -32601 (str "Method not found: " method)))))

(defn- write-response [resp]
  (let [out System/out]
    (.write out (.getBytes (str (json/write-str resp) "\n") "UTF-8"))
    (.flush out)))

(defn -main [& _]
  (let [reader (BufferedReader. (InputStreamReader. System/in))]
    (loop []
      (when-let [line (.readLine reader)]
        (when-not (str/blank? line)
          (try
            (when-let [resp (handle-message (json/read-str line :key-fn keyword))]
              (write-response resp))
            (catch Exception e
              (binding [*out* *err*]
                (println "Error processing message:" (.getMessage e))))))
        (recur)))))
