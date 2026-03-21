(ns steam.format
  "Format Steam API responses into readable text."
  (:require [clojure.string :as str]))

(defn- minutes->hours [m]
  (if (and m (pos? m))
    (format "%.1fh" (/ m 60.0))
    "0h"))

(defn format-player [p]
  (str (:personaname p)
       (when (:realname p) (str " (" (:realname p) ")"))
       " - " (case (:personastate p)
                0 "Offline"
                1 "Online"
                2 "Busy"
                3 "Away"
                4 "Snooze"
                5 "Looking to trade"
                6 "Looking to play"
                "Unknown")
       (when (:gameextrainfo p)
         (str " - Playing: " (:gameextrainfo p)))
       "\n  Profile: " (:profileurl p)))

(defn format-owned-games [{:keys [game_count games]}]
  (let [sorted (->> games
                    (sort-by :playtime_forever #(compare %2 %1)))]
    (str game_count " games owned\n\n"
         "Top by playtime:\n"
         (->> (take 30 sorted)
              (map-indexed
                (fn [i g]
                  (str (inc i) ". " (:name g)
                       " - " (minutes->hours (:playtime_forever g)) " total"
                       (when (pos? (or (:playtime_2weeks g) 0))
                         (str ", " (minutes->hours (:playtime_2weeks g)) " last 2wk"))
                       " [appid:" (:appid g) "]")))
              (str/join "\n")))))

(defn format-recently-played [{:keys [total_count games]}]
  (if (or (nil? games) (zero? (or total_count 0)))
    "No recently played games."
    (str total_count " games played in last 2 weeks:\n\n"
         (->> games
              (map-indexed
                (fn [i g]
                  (str (inc i) ". " (:name g)
                       " - " (minutes->hours (:playtime_2weeks g)) " last 2wk"
                       ", " (minutes->hours (:playtime_forever g)) " total"
                       " [appid:" (:appid g) "]")))
              (str/join "\n")))))

(defn format-friends [friends players]
  (let [player-map (into {} (map (fn [p] [(:steamid p) p]) players))]
    (->> friends
         (map (fn [f]
                (let [p (get player-map (:steamid f))]
                  (if p
                    (format-player p)
                    (str (:steamid f) " - (profile unavailable)")))))
         (str/join "\n\n"))))

(defn format-achievements [{:keys [gameName achievements]}]
  (if (nil? achievements)
    (str gameName ": no achievement data available.")
    (let [achieved (filter #(= 1 (:achieved %)) achievements)
          total (count achievements)]
      (str gameName " - " (count achieved) "/" total " achievements\n\n"
           (->> achievements
                (sort-by :achieved #(compare %2 %1))
                (map (fn [a]
                       (str (if (= 1 (:achieved a)) "[x]" "[ ]")
                            " " (:apiname a)
                            (when (:name a) (str " - " (:name a))))))
                (str/join "\n"))))))

(defn format-news [items appid]
  (if (empty? items)
    (str "No news for appid " appid)
    (->> items
         (map (fn [n]
                (str "## " (:title n)
                     "\n" (:author n)
                     " - " (java.util.Date. (* 1000 (long (:date n))))
                     "\n" (:contents n)
                     (when (:url n) (str "\n" (:url n))))))
         (str/join "\n\n---\n\n"))))

(defn format-game-details [d]
  (when d
    (str (:name d) " [" (:steam_appid d) "]"
         "\n" (:short_description d)
         (when-let [p (:price_overview d)]
           (str "\nPrice: " (:final_formatted p)
                (when (pos? (:discount_percent p))
                  (str " (" (:discount_percent p) "% off)"))))
         (when-let [g (:genres d)]
           (str "\nGenres: " (str/join ", " (map :description g))))
         (when-let [mc (:metacritic d)]
           (str "\nMetacritic: " (:score mc))))))
