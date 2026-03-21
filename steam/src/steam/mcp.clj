(ns steam.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [steam.api :as api]
            [steam.format :as fmt])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "player_summary"
    :description "Get Steam profile info for a player. Defaults to your own profile."
    :inputSchema {:type "object"
                  :properties {:steam_id {:type "string"
                                          :description "Steam ID (optional, defaults to yours)"}}}}
   {:name "owned_games"
    :description "List games owned by a player, sorted by playtime. Defaults to your library."
    :inputSchema {:type "object"
                  :properties {:steam_id {:type "string"
                                          :description "Steam ID (optional, defaults to yours)"}}}}
   {:name "recently_played"
    :description "Games played in the last 2 weeks."
    :inputSchema {:type "object"
                  :properties {:steam_id {:type "string"
                                          :description "Steam ID (optional, defaults to yours)"}}}}
   {:name "friends"
    :description "List friends and their online status. Shows who's online and what they're playing."
    :inputSchema {:type "object"
                  :properties {:steam_id {:type "string"
                                          :description "Steam ID (optional, defaults to yours)"}}}}
   {:name "achievements"
    :description "Get achievement progress for a specific game."
    :inputSchema {:type "object"
                  :properties {:appid {:type "string"
                                       :description "The Steam app ID of the game"}}
                  :required ["appid"]}}
   {:name "game_news"
    :description "Get recent news/updates for a game."
    :inputSchema {:type "object"
                  :properties {:appid {:type "string"
                                       :description "The Steam app ID of the game"}
                               :count {:type "number"
                                       :description "Number of news items (default 5)"}}
                  :required ["appid"]}}
   {:name "game_details"
    :description "Get store page details for a game (description, price, genres, metacritic score)."
    :inputSchema {:type "object"
                  :properties {:appid {:type "string"
                                       :description "The Steam app ID of the game"}}
                  :required ["appid"]}}])

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
     :serverInfo {:name "steam-mcp" :version "0.1.0"}
     :instructions "MCP server for Steam. Set STEAM_API_KEY and STEAM_ID environment variables."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- sid-or-default [args]
  (or (:steam_id args) (System/getenv "STEAM_ID")))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "player_summary"
            (let [sid (sid-or-default arguments)
                  players (api/player-summary sid)]
              (str/join "\n\n" (map fmt/format-player players)))

            "owned_games"
            (let [sid (sid-or-default arguments)]
              (fmt/format-owned-games (api/owned-games sid)))

            "recently_played"
            (let [sid (sid-or-default arguments)]
              (fmt/format-recently-played (api/recently-played sid)))

            "friends"
            (let [sid (sid-or-default arguments)
                  friends (api/friend-list sid)]
              (if (empty? friends)
                "No friends found (profile may be private)."
                (let [ids (->> friends (map :steamid) (str/join ","))
                      players (api/player-summary ids)]
                  (fmt/format-friends friends players))))

            "achievements"
            (fmt/format-achievements (api/achievements (:appid arguments)))

            "game_news"
            (let [n (or (:count arguments) 5)]
              (fmt/format-news (api/game-news (:appid arguments) :count n)
                               (:appid arguments)))

            "game_details"
            (let [d (api/game-details (:appid arguments))]
              (if d
                (fmt/format-game-details d)
                (str "No details found for appid " (:appid arguments))))

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
  (when-not (System/getenv "STEAM_API_KEY")
    (binding [*out* *err*]
      (println "WARNING: STEAM_API_KEY not set - only game_details and game_news will work")))
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
