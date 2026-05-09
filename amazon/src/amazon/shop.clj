(ns amazon.shop
  (:require [amazon.cdp :as cdp]
            [amazon.http :as http]
            [clojure.string :as str])
  (:import [org.jsoup Jsoup]))

(def base-url "https://www.amazon.es")
(defonce ^:private description-cache (atom {}))

(defn- clean [s]
  (some-> s (str/replace #"\s+" " ") str/trim))

(defn- truncate-text [s n]
  (when (seq s)
    (if (<= (count s) n)
      s
      (str (subs s 0 n) "..."))))

(defn- element-texts [doc selector]
  (->> (.select doc selector)
       (map #(clean (.text %)))
       (remove str/blank?)))

(defn- product-description-from-doc [doc]
  (let [bullets (concat
                 (element-texts doc "#feature-bullets li span.a-list-item")
                 (element-texts doc "#pqv-feature-bullets li span.a-list-item")
                 (element-texts doc "#productFactsDesktopExpander li"))
        bullet-text (->> bullets
                         (remove #(re-find #"(?i)informar de un problema|report an issue" %))
                         distinct
                         (take 3)
                         (str/join " "))
        description (or (not-empty bullet-text)
                        (clean (some-> (.selectFirst doc "#productDescription") .text)))]
    (truncate-text description 700)))

(defn- product-description [asin]
  (when (seq asin)
    (if (contains? @description-cache asin)
      (get @description-cache asin)
      (let [description (try
                          (let [resp (http/request (str base-url "/dp/" asin)
                                                   :headers {"Accept" "text/html"})
                                doc (Jsoup/parse ^String (:body resp))]
                            (product-description-from-doc doc))
                          (catch Exception _ nil))]
        (swap! description-cache assoc asin description)
        description))))

(defn search-products
  ([query] (search-products query {}))
  ([query {:keys [include-description include_description max-results max_results]}]
   (let [include-description? (boolean (or include-description include_description))
         max-results (min 10 (max 1 (int (or max-results max_results 8))))
         resp (http/request (str base-url "/s?" (http/encode-query {"k" query}))
                            :headers {"Accept" "text/html"})
         doc (Jsoup/parse ^String (:body resp))
         items (.select doc "[data-component-type=s-search-result]")
         results (->> items
                      (map (fn [item]
                             (let [asin (.attr item "data-asin")
                                   title-el (or (.selectFirst item "h2 a span")
                                                (.selectFirst item "a.a-text-normal span")
                                                (.selectFirst item "h2"))
                                   price-el (or (.selectFirst item ".a-price .a-offscreen")
                                                (.selectFirst item ".a-price"))]
                               {:asin asin
                                :title (clean (some-> title-el .text))
                                :price (clean (some-> price-el .text))
                                :url (when (seq asin) (str base-url "/dp/" asin))})))
                      (filter #(and (seq (:asin %)) (seq (:title %))))
                      (take max-results)
                      vec)]
     (if include-description?
       (mapv #(assoc % :description (product-description (:asin %))) results)
       results))))

(defn- first-result-asin [query]
  (or (some-> (search-products query) first :asin)
      (throw (ex-info (str "No Amazon.es results for query: " query) {}))))

(defn add-to-cart
  [{:keys [asin query quantity] :or {quantity 1}}]
  (let [asin (or asin (first-result-asin query))
        conn (cdp/init-page! (cdp/open-page!))]
    (try
      (cdp/navigate! conn (str base-url "/dp/" asin "/ref=olp-opf-redir?aod=1&ie=UTF8&condition=NEW"))
      (cdp/eval! conn
                 "(function(){const b=[...document.querySelectorAll('button,input')].find(x=>/Aceptar|Accept|Rechazar|Decline/i.test((x.textContent||'')+' '+(x.value||''))); if(b) b.click(); return true;})()")
      (Thread/sleep 1000)
      ;; Quantity is selected by the page's grocery widget. For now, repeat clicks for small quantities.
      (dotimes [_ (max 1 (int quantity))]
        (let [clicked (cdp/eval! conn
                                 "(function(){const sels=['#freshAddToCartButton input','#freshAddToCartButton button','#freshAddToCartButton-announce','input[name=\"submit.add-to-cart\"]','#add-to-cart-button']; for (const s of sels){const e=document.querySelector(s); if(e){e.scrollIntoView({block:'center'}); e.click(); return s;}} return null;})()")]
          (when-not clicked
            (throw (ex-info "No add-to-cart button found on Amazon.es product page" {:asin asin})))
          (Thread/sleep 3500)))
      (let [state (cdp/eval! conn
                             "(function(){return {url:location.href, cart:(document.querySelector('#nav-cart')||{}).innerText, title:(document.querySelector('#productTitle')||{}).innerText || document.title, text:document.body.innerText.slice(0,800)}})()")]
        (str "Added to Amazon.es cart: " asin "\n"
             "Title: " (clean (:title state)) "\n"
             "Cart indicator: " (clean (:cart state))))
      (finally
        (cdp/close! conn)))))

(defn view-cart []
  (let [conn (cdp/init-page! (cdp/open-page!))]
    (try
      (cdp/navigate! conn (str base-url "/gp/cart/view.html"))
      (let [state (cdp/eval! conn
                             "(function(){return {url:location.href, cart:(document.querySelector('#nav-cart')||{}).innerText, text:document.body.innerText.slice(0,2500)}})()")
            text (clean (:text state))]
        (str "Cart indicator: " (clean (:cart state)) "\n\n"
             (subs text 0 (min 2000 (count text)))))
      (finally
        (cdp/close! conn)))))
