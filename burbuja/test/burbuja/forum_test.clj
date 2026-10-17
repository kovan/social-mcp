(ns burbuja.forum-test
  (:require [burbuja.forum]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [org.jsoup Jsoup]))

(defn- extract-content [html]
  (#'burbuja.forum/extract-content
   (.selectFirst (Jsoup/parseBodyFragment html) ".bbWrapper")))

(deftest curl-request-rejects-invalid-url-before-starting-a-process
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Request URL must be a non-blank string"
       (#'burbuja.forum/curl-request nil))))

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
            :error "XenForo returned a non-JSON response"}
           (#'burbuja.forum/successful-reply-response
            "<html><body>Thread page</body></html>")))))

(deftest post-id-from-url-parses-common-burbuja-urls
  (is (= "58327648"
         (#'burbuja.forum/post-id-from-url
          "https://www.burbuja.info/inmobiliaria/posts/58327648/")))
  (is (= "58327648"
         (#'burbuja.forum/post-id-from-url
          "https://www.burbuja.info/inmobiliaria/temas/example.1/post-58327648"))))

(defn- alert-li [html]
  (.selectFirst (Jsoup/parseBodyFragment html) "li"))

(deftest parse-alert-types-defaults-to-quotes-and-thanks
  (testing "omitting the filter keeps the two kinds that need an answer"
    (is (= #{:quote :reaction} (#'burbuja.forum/parse-alert-types nil)))
    (is (= #{:quote :reaction} (#'burbuja.forum/parse-alert-types ""))))

  (testing "accepts strings, keywords and collections, case and space insensitive"
    (is (= #{:reaction} (#'burbuja.forum/parse-alert-types "reaction")))
    (is (= #{:reaction} (#'burbuja.forum/parse-alert-types :reaction)))
    (is (= #{:quote} (#'burbuja.forum/parse-alert-types ["quote"])))
    (is (= #{:quote :reaction} (#'burbuja.forum/parse-alert-types " REACTION , quote "))))

  (testing "all expands to every known kind"
    (is (= #{:quote :reaction :reply :follow :other}
           (#'burbuja.forum/parse-alert-types "all")))))

(deftest alert-type-classifies-the-spanish-wording
  (is (= :quote (#'burbuja.forum/alert-type "X citó tu mensaje en el tema Y.")))
  (is (= :reaction (#'burbuja.forum/alert-type "X reaccionó a tu mensaje en el tema Y con Zanx.")))
  (is (= :reply (#'burbuja.forum/alert-type "X respondió al tema Y.")))
  (is (= :follow (#'burbuja.forum/alert-type "X ahora te está siguiendo.")))
  (is (= :other (#'burbuja.forum/alert-type "X hizo algo que no conocemos."))))

(deftest reaction-name-comes-from-the-emoji-alt-text
  (is (= "Zanx"
         (#'burbuja.forum/reaction-name
          (alert-li (str "<li><span class=\"reaction\">"
                         "<img alt=\"Zanx\" title=\"Zanx\" />"
                         "<span class=\"reaction-text\">Zanx</span></span></li>")))))
  (is (nil? (#'burbuja.forum/reaction-name (alert-li "<li>sin reacción</li>")))))

(deftest alert-thread-title-survives-periods-and-badges
  (testing "a title containing a decimal point is not truncated at it"
    (is (= "URGENTE - Fuerte terremoto sacude Perú - 7.2 en la región andina de Ayacucho"
           (#'burbuja.forum/alert-thread-title
            (alert-li (str "<li><a class=\"username\">Pato</a> reaccionó a "
                           "<a class=\"fauxBlockLink-blockLink\">tu mensaje</a> en el tema "
                           "URGENTE - Fuerte terremoto sacude Perú - 7.2 en la región andina de Ayacucho con "
                           "<span class=\"reaction\"><img alt=\"Zanx\" /></span>."
                           "<div class=\"contentRow-minor\"><time data-short=\"2m\">hace 2 minutos</time></div>"
                           "</li>"))))))

  (testing "the trailing XenForo notice is not mistaken for part of the title"
    (is (= "Se acabó!: los bancos cierran el grifo hipotecario!!"
           (#'burbuja.forum/alert-thread-title
            (alert-li (str "<li><a class=\"username\">eltonelero</a> respondió al tema "
                           "Se acabó!: los bancos cierran el grifo hipotecario!!. "
                           "Puede haber más mensajes después de esto."
                           "<div class=\"contentRow-minor\"><time data-short=\"1h\">hace 1 hora</time></div>"
                           "</li>"))))))

  (testing "an alert with no thread, such as a new follower, yields nothing"
    (is (nil? (#'burbuja.forum/alert-thread-title
               (alert-li "<li><a class=\"username\">X</a> ahora te está siguiendo.</li>"))))))

(defn- article [html]
  (.selectFirst (Jsoup/parseBodyFragment html) "article"))

(defn- article-html [thanks-meta]
  (str "<article data-author=\"selenio\" data-content=\"post-58708859\">"
       "<time datetime=\"2026-08-20T18:58:59+0200\"></time>"
       "<div class=\"bbWrapper\">Texto del mensaje.</div>"
       "<footer class=\"message-footer\">"
       "<div class=\"message-microdata\">" thanks-meta "</div>"
       "</footer></article>"))

(deftest post-thanks-reads-the-running-total-from-microdata
  (testing "the count comes from XenForo's schema.org footer"
    (is (= 4 (#'burbuja.forum/post-thanks
              (article (article-html "<meta itemprop=\"userInteractionCount\" content=\"4\" />"))))))

  (testing "a post nobody thanked reports zero rather than nil"
    (is (= 0 (#'burbuja.forum/post-thanks
              (article (article-html "<meta itemprop=\"userInteractionCount\" content=\"0\" />"))))))

  (testing "missing or unparseable microdata degrades to zero, never to an exception"
    (is (= 0 (#'burbuja.forum/post-thanks (article (article-html "")))))
    (is (= 0 (#'burbuja.forum/post-thanks
              (article (article-html "<meta itemprop=\"userInteractionCount\" content=\"\" />")))))))

(deftest format-post-shows-thanks-only-when-there-are-some
  (testing "a thanked post carries the count in its header"
    (is (str/includes?
         (#'burbuja.forum/format-post
          {:author "selenio" :post-id "58708859" :date "" :number "" :thanks 4 :content "x"})
         "[post:58708859] [thanks:4]")))

  (testing "zero and nil leave the header untouched"
    (doseq [t [0 nil]]
      (let [line (first (str/split-lines
                         (#'burbuja.forum/format-post
                          {:author "selenio" :post-id "58708859" :date "" :number ""
                           :thanks t :content "x"})))]
        (is (= "### selenio [post:58708859]" line))
        (is (not (str/includes? line "thanks")))))))

(deftest alert-epoch-comes-from-data-timestamp-not-data-time
  (testing "data-timestamp is the epoch and is used"
    (is (= 1787263038000
           (#'burbuja.forum/alert-epoch-ms
            (alert-li (str "<li><time data-timestamp=\"1787263038\" data-time=\"11:57 PM\" "
                           "datetime=\"2026-08-20T23:57:18+0200\"></time></li>"))))))

  (testing "data-time alone is a clock reading, never a number, and must not be used"
    ;; Reading it silently produced nil ages, which let the hours filter pass
    ;; every alert through.
    (is (= 1787263038000
           (#'burbuja.forum/alert-epoch-ms
            (alert-li "<li><time data-time=\"11:57 PM\" datetime=\"2026-08-20T23:57:18+0200\"></time></li>")))))

  (testing "no time element at all yields nil rather than throwing"
    (is (nil? (#'burbuja.forum/alert-epoch-ms (alert-li "<li>sin hora</li>"))))
    (is (nil? (#'burbuja.forum/alert-epoch-ms
               (alert-li "<li><time data-time=\"11:57 PM\"></time></li>"))))))

(deftest parse-post-extracts-id-regardless-of-shape
  (testing "thread posts use a data-content=post-N attribute"
    (is (= "58708859" (:post-id (#'burbuja.forum/parse-post (article (article-html "")))))))

  (testing "conversation messages have no data-content, only a #convMessage-N anchor"
    (is (= "1197400"
           (:post-id (#'burbuja.forum/parse-post
                      (article (str "<article data-author=\"selenio\">"
                                    "<span class=\"u-anchorTarget\" id=\"convMessage-1197400\"></span>"
                                    "<time datetime=\"2026-08-20T18:58:59+0200\"></time>"
                                    "<div class=\"bbWrapper\">Hola.</div>"
                                    "</article>"))))))))

(defn- struct-item [html]
  (.selectFirst (Jsoup/parseBodyFragment html) "div"))

;; Fixture shaped like the real markup burbuja.info serves: the title is the
;; anchor itself (a.structItem-title), the conversation route is
;; direct-messages (not conversations), and participants live as a.username
;; links inside .structItem-parts.
(defn- conversation-item-html [& {:keys [unread? href]
                                   :or {unread? true href "/inmobiliaria/direct-messages/tu-naturaleza.373726/unread"}}]
  (str "<div class=\"structItem structItem--conversation" (when unread? " is-unread") "\" data-author=\"Asurbanipal\">"
       "<div class=\"structItem-cell structItem-cell--main\">"
       "<a href=\"" href "\" class=\"structItem-title\">Tu naturaleza</a>"
       "<div class=\"structItem-minor\">"
       "<ul class=\"structItem-parts\"><li>"
       "<ul class=\"listInline\">"
       "<li><a class=\"username\" title=\"Direct message starter\">Asurbanipal</a></li>"
       "<li><a class=\"username\">chatgpt-5.5</a></li>"
       "</ul></li></ul>"
       "</div>"
       "</div>"
       "<div class=\"structItem-cell structItem-cell--latest\">"
       "<time datetime=\"2026-08-22T07:38:30+0200\">Hoy a la(s) 7:38 AM</time>"
       "</div>"
       "</div>"))

(deftest parse-conversation-item-extracts-title-participants-and-unread
  (testing "a normal, unread conversation, with the /unread suffix stripped from the URL"
    (is (= {:title "Tu naturaleza"
            :url "https://www.burbuja.info/inmobiliaria/direct-messages/tu-naturaleza.373726/"
            :participants "Asurbanipal, chatgpt-5.5"
            :latest "2026-08-22T07:38:30+0200"
            :unread? true}
           (#'burbuja.forum/parse-conversation-item
            (struct-item (conversation-item-html))))))

  (testing "a read conversation reports unread? false"
    (is (false? (:unread?
                 (#'burbuja.forum/parse-conversation-item
                  (struct-item (conversation-item-html :unread? false
                                                        :href "/inmobiliaria/direct-messages/otro.1/")))))))

  (testing "an item without a title link is not a conversation row"
    (is (nil? (#'burbuja.forum/parse-conversation-item
               (struct-item "<div class=\"structItem structItem--conversation\">sin link</div>"))))))

(deftest reply-bbcode-skips-native-quote-when-message-quotes
  (let [native (constantly "[QUOTE=\"X, post: 1\"]todo[/QUOTE]")]
    (testing "plain message gets the native quote prepended"
      (is (= "[QUOTE=\"X, post: 1\"]todo[/QUOTE]\n\nhola"
             (#'burbuja.forum/reply-bbcode native "hola"))))
    (testing "message with its own quote is posted as-is"
      (doseq [m ["[QUOTE=\"X, post: 1\"]parte[/QUOTE]\n\nhola"
                 "[quote]parte[/quote] hola"
                 "texto\n[Quote=X]parte[/Quote]"]]
        (is (= m (#'burbuja.forum/reply-bbcode native m)))))
    (testing "native quote is not fetched when not needed"
      (is (= "[QUOTE]a[/QUOTE]"
             (#'burbuja.forum/reply-bbcode
              #(throw (ex-info "should not fetch" {})) "[QUOTE]a[/QUOTE]"))))
    (testing "words starting with quote are not mistaken for tags"
      (is (= "[QUOTE=\"X, post: 1\"]todo[/QUOTE]\n\n[quoted] x"
             (#'burbuja.forum/reply-bbcode native "[quoted] x"))))))
