(ns amazon.mcp
  (:require [amazon.shop :as shop]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def tools
  [{:name "search_amazon"
    :description "Search Amazon Spain (amazon.es) products."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"}}
                  :required ["query"]}}
   {:name "add_to_cart"
    :description "Add an Amazon Spain product to the basket. Use asin when known; otherwise query adds the first search result."
    :inputSchema {:type "object"
                  :properties {:asin {:type "string"}
                               :query {:type "string"}
                               :quantity {:type "number" :default 1}}}}
   {:name "view_cart"
    :description "View the current Amazon Spain basket."
    :inputSchema {:type "object" :properties {}}}])

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
                   "search_amazon" (json/write-str (shop/search-products (:query arguments)))
                   "add_to_cart" (shop/add-to-cart arguments)
                   "view_cart" (shop/view-cart)
                   (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (respond id (tool-result (str "Error: " (.getMessage e)) :error? true)))))

(defn- handle-message [{:keys [id method params]}]
  (case method
    "initialize" (respond id {:protocolVersion "2024-11-05"
                              :capabilities {:tools {}}
                              :serverInfo {:name "amazon-es-mcp" :version "0.1.0"}
                              :instructions "Amazon.es MCP. Uses Chrome cookies and CDP-controlled Chrome for add_to_cart."})
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
