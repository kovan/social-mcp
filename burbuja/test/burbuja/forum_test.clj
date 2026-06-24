(ns burbuja.forum-test
  (:require [burbuja.forum]
            [clojure.test :refer [deftest is testing]])
  (:import [org.jsoup Jsoup]))

(defn- extract-content [html]
  (#'burbuja.forum/extract-content
   (.selectFirst (Jsoup/parseBodyFragment html) ".bbWrapper")))

(deftest extract-content-ignores-lightbox-configuration
  (testing "embedded XenForo lightbox phrases are not treated as post text"
    (let [content
          (extract-content
           (str "<div class=\"bbWrapper\">"
                "<script class=\"js-extraPhrases\" type=\"application/json\">"
                "{\"lightbox_close\":\"Close\","
                "\"lightbox_error\":\"The requested content cannot be loaded.\"}"
                "</script>"
                "<h3>Visible title</h3>"
                "<p>Visible post text.</p>"
                "</div>"))]
      (is (= "Visible title\nVisible post text." content))
      (is (not (re-find #"lightbox_|requested content" content))))))

(deftest extract-content-ignores-other-noncontent-elements
  (testing "styles, templates, and noscript fallbacks are excluded"
    (is (= "Actual post"
           (extract-content
            (str "<div class=\"bbWrapper\">"
                 "<style>.hidden { display: none; }</style>"
                 "<template>Template payload</template>"
                 "<noscript>Fallback payload</noscript>"
                 "Actual post"
                 "</div>"))))))

(deftest quote-from-response-preserves-native-bbcode
  (testing "the XenForo quote is reused verbatim"
    (let [quote
          "[QUOTE=\"Penitentes, post: 58154527, member: 222485\"]\n[B]Texto[/B]\n[/QUOTE]"
          body (str "{\"status\":\"ok\",\"quote\":"
                    (pr-str quote)
                    "}")]
      (is (= quote (#'burbuja.forum/quote-from-response body)))))

  (testing "missing or invalid quote responses are rejected"
    (is (nil? (#'burbuja.forum/quote-from-response "{\"status\":\"ok\"}")))
    (is (nil? (#'burbuja.forum/quote-from-response "not json")))))

(deftest successful-reply-response-detects-xenforo-result
  (testing "successful JSON replies are accepted and expose redirects"
    (is (= {:ok? true
            :redirect "/inmobiliaria/temas/example.1/post-2"
            :message "Posted"}
           (#'burbuja.forum/successful-reply-response
            "{\"status\":\"ok\",\"message\":\"Posted\",\"redirect\":\"/inmobiliaria/temas/example.1/post-2\"}"))))

  (testing "XenForo JSON errors are not treated as success"
    (is (= {:ok? false
            :error "Please wait 30 seconds before performing this action."}
           (#'burbuja.forum/successful-reply-response
            "{\"status\":\"error\",\"errors\":[\"Please wait 30 seconds before performing this action.\"]}"))))

  (testing "unexpected HTML responses are not treated as success"
    (is (= {:ok? false
            :error "Reply returned a non-JSON response"}
           (#'burbuja.forum/successful-reply-response
            "<html><body>Thread page</body></html>")))))
