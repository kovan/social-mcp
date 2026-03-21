(require '[hn.web :as web])
(require '[clojure.string :as str])

(let [resp (#'web/get-page "https://news.ycombinator.com/reply?id=47390817")
      doc (org.jsoup.Jsoup/parse ^String (:body resp))]
  (println "Status:" (:status resp))
  (println "Forms:" (count (.select doc "form")))
  (doseq [f (.select doc "form")]
    (println "  action:" (.attr f "action"))
    (println "  inputs:" (mapv #(.attr % "name") (.select f "input"))))
  (when (zero? (count (.select doc "form")))
    (println "Body preview:" (subs (:body resp) 0 (min 2000 (count (:body resp)))))))
