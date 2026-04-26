(ns amazon.shop
  (:require [amazon.cdp :as cdp]
            [amazon.http :as http]
            [clojure.string :as str])
  (:import [org.jsoup Jsoup]))

(def base-url "https://www.amazon.es")

(defn- clean [s]
  (some-> s (str/replace #"\s+" " ") str/trim))

(defn search-products [query]
  (let [resp (http/request (str base-url "/s?" (http/encode-query {"k" query}))
                           :headers {"Accept" "text/html"})
        doc (Jsoup/parse ^String (:body resp))
        items (.select doc "[data-component-type=s-search-result]")]
    (->> items
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
         (take 8)
         vec)))

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
