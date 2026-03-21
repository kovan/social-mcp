(ns steam.api
  "Steam Web API client."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(def ^:private base "https://api.steampowered.com")
(def ^:private store-base "https://store.steampowered.com/api")

(def ^:private api-key (System/getenv "STEAM_API_KEY"))
(def ^:private steam-id (System/getenv "STEAM_ID"))

(defn- fetch-json [url]
  (let [pb (ProcessBuilder. ["curl" "-sSL" url])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- api-url [interface method version params]
  (let [base-params (cond-> {:key api-key :format "json"}
                      steam-id (assoc :steamid steam-id))
        all-params (merge base-params params)
        query (->> all-params
                   (remove (comp nil? val))
                   (map (fn [[k v]] (str (name k) "=" (java.net.URLEncoder/encode (str v) "UTF-8"))))
                   (str/join "&"))]
    (str base "/" interface "/" method "/v" version "/?" query)))

(defn player-summary
  "Get profile info for one or more Steam IDs (comma-separated)."
  ([] (player-summary steam-id))
  ([ids]
   (let [url (api-url "ISteamUser" "GetPlayerSummaries" 2
               {:steamids ids})]
     (-> (fetch-json url) :response :players))))

(defn owned-games
  "Get all games owned by a player."
  ([] (owned-games steam-id))
  ([sid]
   (let [url (api-url "IPlayerService" "GetOwnedGames" 1
               {:steamid sid
                :include_appinfo "true"
                :include_played_free_games "true"})]
     (-> (fetch-json url) :response))))

(defn recently-played
  "Get recently played games (last 2 weeks)."
  ([] (recently-played steam-id))
  ([sid]
   (let [url (api-url "IPlayerService" "GetRecentlyPlayedGames" 1
               {:steamid sid :count 20})]
     (-> (fetch-json url) :response))))

(defn friend-list
  "Get friend list for a player."
  ([] (friend-list steam-id))
  ([sid]
   (let [url (api-url "ISteamUser" "GetFriendList" 1
               {:steamid sid :relationship "friend"})]
     (-> (fetch-json url) :friendslist :friends))))

(defn achievements
  "Get player achievements for a game."
  [appid]
  (let [url (api-url "ISteamUserStats" "GetPlayerAchievements" 1
              {:appid appid :steamid steam-id})]
    (-> (fetch-json url) :playerstats)))

(defn game-news
  "Get news for a game."
  [appid & {:keys [count] :or {count 5}}]
  (let [url (api-url "ISteamNews" "GetNewsForApp" 2
              {:appid appid :count count :maxlength 500})]
    (-> (fetch-json url) :appnews :newsitems)))

(defn game-details
  "Get store details for a game (uses store API, no key needed)."
  [appid]
  (let [url (str store-base "/appdetails?appids=" appid)]
    (-> (fetch-json url)
        ((keyword (str appid)))
        :data)))
