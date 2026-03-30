(ns ibkr-mcp.connection
  "IBKR TWS API connection management.
   Uses a global atom for state and promise-based async request/response."
  (:require [clojure.string :as str])
  (:import [com.ib.client EClientSocket EJavaSignal EReader Contract Order Decimal
                          DefaultEWrapper ScannerSubscription TagValue WshEventData]))

;; ---------------------------------------------------------------------------
;; State

(defonce state
  (atom {:client       nil
         :signal       nil
         :connected?   false
         :next-order-id nil
         :next-req-id  0
         :pending      {}}))

(defn next-req-id! []
  (:next-req-id (swap! state update :next-req-id inc)))

;; ---------------------------------------------------------------------------
;; Promise-based async request management

(defn- register-request! [req-id]
  (let [p (promise)
        data (atom [])]
    (swap! state assoc-in [:pending req-id] {:promise p :data data})
    p))

(defn- append-data! [req-id item]
  (when-let [pending (get-in @state [:pending req-id])]
    (swap! (:data pending) conj item)))

(defn- complete-request! [req-id]
  (when-let [pending (get-in @state [:pending req-id])]
    (deliver (:promise pending) @(:data pending))
    (swap! state update :pending dissoc req-id)))

(defn- complete-single! [req-id value]
  (when-let [pending (get-in @state [:pending req-id])]
    (deliver (:promise pending) value)
    (swap! state update :pending dissoc req-id)))

(defn- fail-request! [req-id error-msg]
  (when-let [pending (get-in @state [:pending req-id])]
    (deliver (:promise pending) {:error error-msg})
    (swap! state update :pending dissoc req-id)))

(defn await-response
  "Deref a request promise with a timeout (default 30s)."
  [p & {:keys [timeout] :or {timeout 30000}}]
  (let [result (deref p timeout ::timeout)]
    (if (= result ::timeout)
      {:error "Request timed out"}
      result)))

;; ---------------------------------------------------------------------------
;; Tick field decoders

(def price-fields
  {1 "Bid" 2 "Ask" 4 "Last" 6 "High" 7 "Low" 9 "Close" 14 "Open"})

(def size-fields
  {0 "Bid Size" 3 "Ask Size" 5 "Last Size" 8 "Volume"})

;; ---------------------------------------------------------------------------
;; EWrapper

(defn- make-wrapper []
  (proxy [DefaultEWrapper] []

    ;; Connection
    (nextValidId [order-id]
      (swap! state assoc :next-order-id order-id :connected? true))

    (connectAck []
      (let [client (:client @state)]
        (when (and client (.isAsyncEConnect ^EClientSocket client))
          (.startAPI ^EClientSocket client))))

    (connectionClosed []
      (swap! state assoc :connected? false)
      (binding [*out* *err*] (println "IBKR connection closed")))

    ;; Error — TWS API 10.x: error(int id, long reqId, int code, String msg, String json)
    (error
      ([arg]
       (binding [*out* *err*]
         (if (instance? Exception arg)
           (println "IBKR error:" (.getMessage ^Exception arg))
           (println "IBKR error:" arg))))
      ([id req-id code msg _json]
       (binding [*out* *err*]
         (println (str "IBKR error " code ": " msg)))
       (when (pos? (int id)) (fail-request! (int id) (str "Error " code ": " msg)))))

    ;; Market data ticks
    (tickPrice [req-id field price _attrib]
      (append-data! req-id {:type :price :field field :value price}))

    (tickSize [req-id field size]
      (append-data! req-id {:type :size :field field :value (str size)}))

    (tickString [req-id tick-type value]
      (append-data! req-id {:type :string :field tick-type :value value}))

    (tickSnapshotEnd [req-id]
      (complete-request! req-id))

    ;; Fundamental data (XML)
    (fundamentalData [req-id data]
      (complete-single! req-id data))

    ;; Historical data
    (historicalData [req-id bar]
      (append-data! req-id {:time  (.time bar)
                            :open  (.open bar)
                            :high  (.high bar)
                            :low   (.low bar)
                            :close (.close bar)
                            :volume (str (.volume bar))
                            :wap    (str (.wap bar))}))

    (historicalDataEnd [req-id _start _end]
      (complete-request! req-id))

    ;; Positions (no req-id — uses separate promise)
    (position [account contract pos avg-cost]
      (swap! state update :position-acc (fnil conj [])
             {:account  account
              :symbol   (.symbol contract)
              :sec-type (str (.secType contract))
              :exchange (.exchange contract)
              :currency (.currency contract)
              :position (str pos)
              :avg-cost avg-cost}))

    (positionEnd []
      (when-let [p (:positions-promise @state)]
        (deliver p (or (:position-acc @state) []))
        (swap! state dissoc :positions-promise :position-acc)))

    ;; Account summary
    (accountSummary [req-id account tag value currency]
      (append-data! req-id {:account account :tag tag :value value :currency currency}))

    (accountSummaryEnd [req-id]
      (complete-request! req-id))

    ;; Open orders (no req-id — uses separate promise)
    (openOrder [order-id contract order order-state]
      ;; Check if this is a what-if response
      (if (.whatIf order)
        (let [what-if-data {:order-id         order-id
                            :symbol           (.symbol contract)
                            :init-margin      (.initMarginAfter order-state)
                            :maint-margin     (.maintMarginAfter order-state)
                            :equity-with-loan (.equityWithLoanAfter order-state)
                            :commission       (.commission order-state)
                            :commission-ccy   (.commissionCurrency order-state)
                            :warning          (.warningText order-state)}
              ;; Find the pending what-if request
              match (->> (:pending @state)
                         (filter (fn [[_ v]] (= order-id (:what-if-order-id v))))
                         first)]
          (when match
            (complete-single! (first match) what-if-data)))
        ;; Normal open order
        (swap! state update :order-acc (fnil conj [])
               {:order-id    order-id
                :symbol      (.symbol contract)
                :sec-type    (str (.secType contract))
                :action      (str (.action order))
                :quantity    (str (.totalQuantity order))
                :order-type  (str (.orderType order))
                :limit-price (.lmtPrice order)
                :status      (.status order-state)})))

    (openOrderEnd []
      (when-let [p (:orders-promise @state)]
        (deliver p (or (:order-acc @state) []))
        (swap! state dissoc :orders-promise :order-acc)))

    (orderStatus [order-id status _filled _remaining _avg-fill
                  _perm-id _parent-id _last-fill _client-id _why-held _mkt-cap]
      (binding [*out* *err*]
        (println "Order" order-id "status:" status)))

    ;; Contract details
    (contractDetails [req-id details]
      (let [c (.contract details)]
        (append-data! req-id
                      {:symbol      (.symbol c)
                       :sec-type    (str (.secType c))
                       :exchange    (.exchange c)
                       :primary-exch (.primaryExch c)
                       :currency    (.currency c)
                       :long-name   (.longName details)
                       :industry    (.industry details)
                       :category    (.category details)
                       :subcategory (.subcategory details)
                       :min-tick    (.minTick details)})))

    (contractDetailsEnd [req-id]
      (complete-request! req-id))

    ;; Symbol search
    (symbolSamples [req-id descriptions]
      (complete-single!
       req-id
       (mapv (fn [desc]
               (let [c (.contract desc)]
                 {:symbol     (.symbol c)
                  :sec-type   (str (.secType c))
                  :currency   (.currency c)
                  :primary-exch (.primaryExch c)}))
             descriptions)))

    ;; Scanner
    (scannerParameters [xml]
      (when-let [p (:scanner-params-promise @state)]
        (deliver p xml)
        (swap! state dissoc :scanner-params-promise)))

    (scannerData [req-id rank details distance benchmark projection legs-str]
      (append-data! req-id
                    {:rank     rank
                     :symbol   (.symbol (.contract details))
                     :sec-type (str (.secType (.contract details)))
                     :exchange (.exchange (.contract details))
                     :currency (.currency (.contract details))
                     :long-name (.longName details)
                     :distance  distance
                     :benchmark benchmark
                     :projection projection}))

    (scannerDataEnd [req-id]
      (complete-request! req-id))

    ;; News
    (newsProviders [providers]
      (when-let [p (:news-providers-promise @state)]
        (deliver p (mapv (fn [np] {:code (.providerCode np) :name (.providerName np)})
                         providers))
        (swap! state dissoc :news-providers-promise)))

    (historicalNews [req-id time provider-code article-id headline]
      (append-data! req-id {:time time :provider provider-code
                            :article-id article-id :headline headline}))

    (historicalNewsEnd [req-id has-more]
      (complete-request! req-id))

    (newsArticle [req-id article-type article-text]
      (complete-single! req-id {:type article-type :text article-text}))

    ;; Options chain
    (securityDefinitionOptionalParameter [req-id exchange underlying-con-id
                                          trading-class multiplier expirations strikes]
      (append-data! req-id {:exchange      exchange
                            :trading-class trading-class
                            :multiplier    multiplier
                            :expirations   (sort (vec expirations))
                            :strikes       (sort (vec strikes))}))

    (securityDefinitionOptionalParameterEnd [req-id]
      (complete-request! req-id))

    ;; Wall Street Horizon
    (wshMetaData [req-id data-json]
      (complete-single! req-id data-json))

    (wshEventData [req-id data-json]
      (complete-single! req-id data-json))))

;; ---------------------------------------------------------------------------
;; Connection

(defn connected? [] (:connected? @state))

(defn connect!
  [{:keys [host port client-id]
    :or   {host "127.0.0.1" port 7497 client-id 1}}]
  (if (connected?)
    {:status :already-connected}
    (let [signal  (EJavaSignal.)
          wrapper (make-wrapper)
          client  (EClientSocket. wrapper signal)]
      (swap! state assoc :client client :signal signal :connected? false)
      (.eConnect client host (int port) (int client-id))
      (let [reader (EReader. client signal)]
        (.start reader)
        (future
          (while (.isConnected client)
            (.waitForSignal signal)
            (.processMsgs reader))))
      ;; Wait for nextValidId callback
      (Thread/sleep 2000)
      (if (connected?)
        {:status :connected :next-order-id (:next-order-id @state)}
        (do (try (.eDisconnect client) (catch Exception _))
            (swap! state assoc :client nil :signal nil)
            {:status :failed :message "Connection timed out. Is TWS/Gateway running with API enabled?"})))))

(defn disconnect! []
  (when-let [client (:client @state)]
    (try (.eDisconnect ^EClientSocket client) (catch Exception _)))
  (swap! state assoc :connected? false :client nil :signal nil)
  {:status :disconnected})

;; Shutdown hook
(.addShutdownHook (Runtime/getRuntime)
                  (Thread. ^Runnable #(when (connected?) (disconnect!))))

;; ---------------------------------------------------------------------------
;; Contract / Order helpers

(defn make-contract
  [{:keys [symbol sec-type exchange currency con-id]
    :or   {sec-type "STK" exchange "SMART" currency "USD"}}]
  (let [c (Contract.)]
    (.symbol c symbol)
    (.secType c sec-type)
    (.exchange c exchange)
    (.currency c currency)
    (when con-id (.conid c (int con-id)))
    c))

(defn- client ^EClientSocket [] (:client @state))

;; ---------------------------------------------------------------------------
;; Request functions

(defn request-quote
  "Snapshot quote. Returns promise of tick data."
  [contract-opts]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqMktData (client) id (make-contract contract-opts) "" true false [])
    p))

(defn request-fundamental-ratios
  "Snapshot with fundamental ratios (tick 258). Returns promise of tick data."
  [contract-opts]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqMktData (client) id (make-contract contract-opts) "258" true false [])
    p))

(defn request-fundamental-data
  "Full fundamental data report. report-type is one of:
   ReportsFinSummary, ReportsFinStatements, ReportSnapshot,
   ReportsOwnership, RESC, CalendarReport.
   Returns promise of XML string."
  [contract-opts report-type]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqFundamentalData (client) id (make-contract contract-opts) report-type [])
    p))

(defn request-historical-data
  [contract-opts {:keys [end-date duration bar-size what-to-show use-rth]
                  :or   {end-date "" duration "1 M" bar-size "1 day"
                         what-to-show "MIDPOINT" use-rth 1}}]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqHistoricalData (client) id (make-contract contract-opts)
                        end-date duration bar-size what-to-show
                        (int use-rth) 1 false [])
    p))

(defn request-positions []
  (let [p (promise)]
    (swap! state assoc :positions-promise p :position-acc [])
    (.reqPositions (client))
    p))

(defn request-account-summary
  "tags: comma-separated, e.g. \"NetLiquidation,TotalCashValue,BuyingPower\""
  [tags]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqAccountSummary (client) id "All" tags)
    p))

(defn request-open-orders []
  (let [p (promise)]
    (swap! state assoc :orders-promise p :order-acc [])
    (.reqAllOpenOrders (client))
    p))

(defn place-order!
  [contract-opts {:keys [action quantity cash-qty order-type limit-price]
                  :or   {order-type "MKT"}}]
  (let [order-id (:next-order-id @state)
        contract (make-contract contract-opts)
        order    (doto (Order.)
                   (.action action)
                   (.orderType order-type))]
    (if cash-qty
      (.cashQty order (double cash-qty))
      (.totalQuantity order (Decimal/get (double quantity))))
    (when limit-price
      (.lmtPrice order (double limit-price)))
    (.placeOrder (client) order-id contract order)
    (swap! state update :next-order-id inc)
    {:order-id order-id :symbol (:symbol contract-opts)
     :action action :quantity (or quantity cash-qty) :order-type order-type
     :cash-qty (boolean cash-qty)}))

(defn cancel-order! [order-id]
  (.cancelOrder (client) (int order-id) "")
  {:cancelled order-id})

(defn search-contracts [pattern]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqMatchingSymbols (client) id pattern)
    p))

(defn request-contract-details [contract-opts]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqContractDetails (client) id (make-contract contract-opts))
    p))

;; ---------------------------------------------------------------------------
;; Scanner

(defn request-scanner-parameters
  "Get XML describing all available scan codes and filters."
  []
  (let [p (promise)]
    (swap! state assoc :scanner-params-promise p)
    (.reqScannerParameters (client))
    p))

(defn request-scanner
  "Run a market scanner. Returns promise of ranked results.
   scan-code: e.g. TOP_PERC_GAIN, MOST_ACTIVE, HOT_BY_VOLUME
   location: e.g. STK.US.MAJOR, STK.US, STK.EU
   Options: :num-rows, :above-price, :below-price, :above-volume,
            :market-cap-above, :instrument (STK), :stock-type (ALL/ETF/CORP)"
  [{:keys [scan-code location num-rows instrument stock-type
           above-price below-price above-volume market-cap-above]
    :or   {location "STK.US.MAJOR" num-rows 20 instrument "STK" stock-type "ALL"}}]
  (let [id  (next-req-id!)
        p   (register-request! id)
        sub (doto (ScannerSubscription.)
              (.scanCode scan-code)
              (.locationCode location)
              (.numberOfRows (int num-rows))
              (.instrument instrument)
              (.stockTypeFilter stock-type))]
    (when above-price (.abovePrice sub (double above-price)))
    (when below-price (.belowPrice sub (double below-price)))
    (when above-volume (.aboveVolume sub (int above-volume)))
    (when market-cap-above (.marketCapAbove sub (double market-cap-above)))
    (.reqScannerSubscription (client) id sub [] [])
    ;; Cancel after results arrive (scanner is a subscription)
    (future
      (deref p 30000 nil)
      (try (.cancelScannerSubscription (client) id) (catch Exception _)))
    p))

;; ---------------------------------------------------------------------------
;; News

(defn request-news-providers
  "Get list of available news providers."
  []
  (let [p (promise)]
    (swap! state assoc :news-providers-promise p)
    (.reqNewsProviders (client))
    p))

(defn request-historical-news
  "Get historical news headlines for a contract.
   provider-codes: comma-separated, e.g. \"BRFG,BRFUPDN,DJNL\"
   start/end: \"yyyy-MM-dd HH:mm:ss.0\" format"
  [{:keys [con-id provider-codes start end total]
    :or   {provider-codes "BRFG,BRFUPDN,DJNL" start "" end "" total 30}}]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqHistoricalNews (client) id (int con-id) provider-codes start end (int total) [])
    p))

(defn request-news-article
  "Get the full text of a news article."
  [provider-code article-id]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqNewsArticle (client) id provider-code article-id [])
    p))

;; ---------------------------------------------------------------------------
;; Options chains

(defn request-option-chain
  "Get available expirations and strikes for an underlying.
   Requires con-id of the underlying."
  [{:keys [symbol sec-type con-id exchange]
    :or   {sec-type "STK" exchange ""}}]
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqSecDefOptParams (client) id symbol exchange sec-type (int con-id))
    p))

;; ---------------------------------------------------------------------------
;; What-if orders

(defn what-if-order!
  "Check margin impact without actually placing an order."
  [contract-opts {:keys [action quantity order-type limit-price]
                  :or   {order-type "MKT"}}]
  (let [order-id (:next-order-id @state)
        contract (make-contract contract-opts)
        order    (doto (Order.)
                   (.action action)
                   (.totalQuantity (Decimal/get (double quantity)))
                   (.orderType order-type)
                   (.whatIf true))
        id       (next-req-id!)
        p        (register-request! id)]
    (when limit-price (.lmtPrice order (double limit-price)))
    ;; Override openOrder to capture what-if result
    (swap! state assoc-in [:pending id :what-if-order-id] order-id)
    (.placeOrder (client) order-id contract order)
    (swap! state update :next-order-id inc)
    p))

;; ---------------------------------------------------------------------------
;; Wall Street Horizon

(defn request-wsh-meta
  "Get Wall Street Horizon metadata (available event types)."
  []
  (let [id (next-req-id!)
        p  (register-request! id)]
    (.reqWshMetaData (client) id)
    p))

(defn request-wsh-events
  "Get Wall Street Horizon events for a contract or filter.
   start/end: YYYYMMDD format"
  [{:keys [con-id start end limit]
    :or   {limit 50}}]
  (let [id  (next-req-id!)
        p   (register-request! id)
        evt (WshEventData. (int (or con-id Integer/MAX_VALUE))
                           false false false
                           (or start "") (or end "")
                           (int limit))]
    (.reqWshEventData (client) id evt)
    p))
