(ns burbuja.mcp-test
  (:require [burbuja.mcp]
            [clojure.test :refer [deftest is testing]]))

(deftest tool-call-validates-required-arguments-before-dispatch
  (testing "a missing post URL returns a concise tool error without reaching curl"
    (let [response (#'burbuja.mcp/handle-tools-call
                    1
                    {:name "reply_comment"
                     :arguments {:message "Reply text"}})
          result (:result response)
          error-text (get-in result [:content 0 :text])]
      (is (true? (:isError result)))
      (is (= "Error: Missing required argument for reply_comment: post_url"
             error-text))
      (is (not (re-find #"NullPointerException|ProcessBuilder" error-text)))))

  (testing "blank required values are rejected as missing"
    (let [response (#'burbuja.mcp/handle-tools-call
                    2
                    {:name "reply_comment"
                     :arguments {:post_url "  " :message ""}})
          error-text (get-in response [:result :content 0 :text])]
      (is (= "Error: Missing required arguments for reply_comment: post_url, message"
             error-text)))))
