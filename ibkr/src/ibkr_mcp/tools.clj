(ns ibkr-mcp.tools
  "MCP tool definitions for IBKR integration."
  (:require [clojure.string :as str]
            [ibkr-mcp.connection :as conn]
            [ibkr-mcp.yahoo :as yahoo]))

;; ---------------------------------------------------------------------------
;; Response helpers

(defn- success [text]
  {:content [{:type "text" :text (str text)}] :isError false})

(defn- failure [text]
  {:content [{:type "text" :text (str text)}] :isError true})

(defn- safe-call [f]
  (try (f)
       (catch Exception e
         (failure (str "Error: " (.getMessage e))))))

;; ---------------------------------------------------------------------------
;; Formatting

(defn- format-ticks
  "Format tick data from a market data snapshot into readable text."
  [ticks]
  (let [prices (->> ticks
                    (filter #(= :price (:type %)))
                    (keep (fn [{:keys [field value]}]
                            (when-let [label (conn/price-fields field)]
                              (format "%s: %.4f" label (double value))))))
        sizes  (->> ticks
                    (filter #(= :size (:type %)))
                    (keep (fn [{:keys [field value]}]
                            (when-let [label (conn/size-fields field)]
                              (str label ": " value)))))]
    (str/join "\n" (concat prices sizes))))

(defn- parse-ratio-string
  "Parse 'KEY=val;KEY=val;...' into a sorted map."
  [s]
  (when s
    (->> (str/split s #";")
         (keep (fn [pair]
                 (let [[k v] (str/split pair #"=" 2)]
                   (when (and k v (not= v ""))
                     [k v]))))
         (sort-by first)
         (map (fn [[k v]] (str k "=" v)))
         (str/join "\n"))))

(defn- format-bars [bars]
  (str/join "\n"
            (cons "Date | Open | High | Low | Close | Volume"
                  (map (fn [{:keys [time open high low close volume]}]
                         (format "%s | %.2f | %.2f | %.2f | %.2f | %s"
                                 time (double open) (double high)
                                 (double low) (double close) volume))
                       bars))))

;; ---------------------------------------------------------------------------
;; Tool definitions

(def connect-tool
  {:name "ibkr_connect"
   :description "Connect to IBKR TWS or IB Gateway. Ports: 7497=TWS paper, 7496=TWS live, 4002=Gateway paper, 4001=Gateway live."
   :inputSchema {:type "object"
                 :properties {:host      {:type "string" :description "Host (default 127.0.0.1)"}
                              :port      {:type "integer" :description "Port number"}
                              :client_id {:type "integer" :description "Client ID (default 1)"}}
                 :required [:port]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [result (conn/connect! {:host      (or (:host args) "127.0.0.1")
                                             :port      (:port args)
                                             :client-id (or (:client_id args) 1)})]
                  (if (= :failed (:status result))
                    (failure (:message result "Connection failed"))
                    (success (pr-str result))))))})

(def disconnect-tool
  {:name "ibkr_disconnect"
   :description "Disconnect from IBKR."
   :inputSchema {:type "object" :properties {}}
   :tool-fn (fn [_ctx _args]
              (safe-call #(success (pr-str (conn/disconnect!)))))})

(def status-tool
  {:name "ibkr_status"
   :description "Check IBKR connection status."
   :inputSchema {:type "object" :properties {}}
   :tool-fn (fn [_ctx _args]
              (success (str "Connected: " (conn/connected?)
                            (when (conn/connected?)
                              (str "\nNext order ID: " (:next-order-id @conn/state))))))})

(def quote-tool
  {:name "get_quote"
   :description "Get a market data snapshot (bid, ask, last, high, low, close, volume) for a symbol."
   :inputSchema {:type "object"
                 :properties {:symbol   {:type "string" :description "Ticker symbol (e.g. AAPL)"}
                              :sec_type {:type "string" :description "STK, OPT, FUT, CASH, BOND (default STK)"}
                              :exchange {:type "string" :description "Exchange (default SMART)"}
                              :currency {:type "string" :description "Currency (default USD)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [ticks (conn/await-response
                             (conn/request-quote
                              {:symbol   (:symbol args)
                               :sec-type (or (:sec_type args) "STK")
                               :exchange (or (:exchange args) "SMART")
                               :currency (or (:currency args) "USD")}))]
                  (if (:error ticks)
                    (failure (:error ticks))
                    (success (str (:symbol args) " Quote:\n" (format-ticks ticks)))))))})

(def fundamental-ratios-tool
  {:name "get_fundamental_ratios"
   :description "Get key fundamental ratios (P/E, market cap, EPS, beta, book value, etc.) for a stock. Faster than get_fundamentals but fewer data points."
   :inputSchema {:type "object"
                 :properties {:symbol   {:type "string" :description "Ticker symbol"}
                              :exchange {:type "string" :description "Exchange (default SMART)"}
                              :currency {:type "string" :description "Currency (default USD)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [ticks (conn/await-response
                             (conn/request-fundamental-ratios
                              {:symbol   (:symbol args)
                               :exchange (or (:exchange args) "SMART")
                               :currency (or (:currency args) "USD")}))
                      ratio-str (->> ticks
                                     (filter (fn [{:keys [type field]}]
                                               (and (= type :string) (= field 47))))
                                     first :value)]
                  (if (:error ticks)
                    (failure (:error ticks))
                    (if ratio-str
                      (success (str (:symbol args) " Fundamental Ratios:\n"
                                    (parse-ratio-string ratio-str)))
                      (success (str (:symbol args) " — no fundamental ratios available")))))))})

(def fundamentals-tool
  {:name "get_fundamentals"
   :description "Get full fundamental data report for a stock (financial statements, analyst estimates, ownership, etc.). Returns detailed XML data. report_type options: ReportsFinSummary (financial summary), ReportsFinStatements (income/balance/cash flow), ReportSnapshot (company overview), ReportsOwnership (ownership info), RESC (analyst estimates), CalendarReport (earnings calendar)."
   :inputSchema {:type "object"
                 :properties {:symbol      {:type "string" :description "Ticker symbol"}
                              :report_type {:type "string" :description "Report type (see description)"}
                              :exchange    {:type "string" :description "Exchange (default SMART)"}
                              :currency    {:type "string" :description "Currency (default USD)"}}
                 :required [:symbol :report_type]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [xml (conn/await-response
                           (conn/request-fundamental-data
                            {:symbol   (:symbol args)
                             :exchange (or (:exchange args) "SMART")
                             :currency (or (:currency args) "USD")}
                            (:report_type args))
                           :timeout 60000)]
                  (if (:error xml)
                    (failure (:error xml))
                    (success xml)))))})

(def historical-data-tool
  {:name "get_historical_data"
   :description "Get historical price bars. Duration examples: '1 D', '1 W', '1 M', '3 M', '1 Y'. Bar sizes: '1 min', '5 mins', '15 mins', '1 hour', '1 day', '1 week'. what_to_show: MIDPOINT, TRADES, BID, ASK."
   :inputSchema {:type "object"
                 :properties {:symbol       {:type "string" :description "Ticker symbol"}
                              :sec_type     {:type "string" :description "STK, OPT, FUT, CASH (default STK)"}
                              :duration     {:type "string" :description "Duration (default '1 M')"}
                              :bar_size     {:type "string" :description "Bar size (default '1 day')"}
                              :what_to_show {:type "string" :description "Data type (default MIDPOINT)"}
                              :exchange     {:type "string" :description "Exchange (default SMART)"}
                              :currency     {:type "string" :description "Currency (default USD)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [bars (conn/await-response
                            (conn/request-historical-data
                             {:symbol   (:symbol args)
                              :sec-type (or (:sec_type args) "STK")
                              :exchange (or (:exchange args) "SMART")
                              :currency (or (:currency args) "USD")}
                             {:duration     (or (:duration args) "1 M")
                              :bar-size     (or (:bar_size args) "1 day")
                              :what-to-show (or (:what_to_show args) "MIDPOINT")})
                            :timeout 60000)]
                  (if (:error bars)
                    (failure (:error bars))
                    (success (str (:symbol args) " Historical Data:\n"
                                  (format-bars bars)))))))})

(def positions-tool
  {:name "get_positions"
   :description "Get all current positions across all accounts."
   :inputSchema {:type "object" :properties {}}
   :tool-fn (fn [_ctx _args]
              (safe-call
               #(let [positions (conn/await-response (conn/request-positions))]
                  (if (:error positions)
                    (failure (:error positions))
                    (if (empty? positions)
                      (success "No open positions.")
                      (success (str/join "\n\n"
                                         (map (fn [p]
                                                (str/join "\n"
                                                          [(str "Symbol: " (:symbol p))
                                                           (str "Type: " (:sec-type p))
                                                           (str "Position: " (:position p))
                                                           (str "Avg Cost: " (:avg-cost p))
                                                           (str "Account: " (:account p))]))
                                              positions))))))))})

(def account-summary-tool
  {:name "get_account_summary"
   :description "Get account summary. Common tags: NetLiquidation, TotalCashValue, BuyingPower, GrossPositionValue, AvailableFunds, MaintMarginReq, ExcessLiquidity, Cushion."
   :inputSchema {:type "object"
                 :properties {:tags {:type "string"
                                     :description "Comma-separated tags (default: all common tags)"}}
                 :required []}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [tags (or (:tags args)
                               "NetLiquidation,TotalCashValue,BuyingPower,GrossPositionValue,AvailableFunds,MaintMarginReq,ExcessLiquidity,Cushion")
                      summary (conn/await-response (conn/request-account-summary tags))]
                  (if (:error summary)
                    (failure (:error summary))
                    (success (str/join "\n"
                                       (map (fn [{:keys [tag value currency]}]
                                              (str tag ": " value " " currency))
                                            summary)))))))})

(def open-orders-tool
  {:name "get_open_orders"
   :description "Get all open/pending orders."
   :inputSchema {:type "object" :properties {}}
   :tool-fn (fn [_ctx _args]
              (safe-call
               #(let [orders (conn/await-response (conn/request-open-orders))]
                  (if (:error orders)
                    (failure (:error orders))
                    (if (empty? orders)
                      (success "No open orders.")
                      (success (str/join "\n\n"
                                         (map (fn [o]
                                                (str/join "\n"
                                                          [(str "Order #" (:order-id o))
                                                           (str "Symbol: " (:symbol o))
                                                           (str "Action: " (:action o))
                                                           (str "Qty: " (:quantity o))
                                                           (str "Type: " (:order-type o))
                                                           (str "Limit: " (:limit-price o))
                                                           (str "Status: " (:status o))]))
                                              orders))))))))})

(def place-order-tool
  {:name "place_order"
   :description "Place a trading order. WARNING: This submits a real order to the connected IBKR account (paper or live). Verify all parameters carefully. Use cash_qty for fractional share orders (specify dollar amount instead of share count)."
   :inputSchema {:type "object"
                 :properties {:symbol      {:type "string" :description "Ticker symbol"}
                              :sec_type    {:type "string" :description "STK, OPT, FUT, CASH (default STK)"}
                              :action      {:type "string" :description "BUY or SELL"}
                              :quantity    {:type "number" :description "Number of shares/contracts (use this OR cash_qty, not both)"}
                              :cash_qty    {:type "number" :description "Dollar amount to buy/sell (enables fractional shares). Use this OR quantity, not both."}
                              :order_type  {:type "string" :description "MKT, LMT, STP, STP_LMT (default MKT)"}
                              :limit_price {:type "number" :description "Limit price (required for LMT orders)"}
                              :exchange    {:type "string" :description "Exchange (default SMART)"}
                              :currency    {:type "string" :description "Currency (default USD)"}}
                 :required [:symbol :action]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [result (conn/place-order!
                              {:symbol   (:symbol args)
                               :sec-type (or (:sec_type args) "STK")
                               :exchange (or (:exchange args) "SMART")
                               :currency (or (:currency args) "USD")}
                              {:action      (:action args)
                               :quantity    (:quantity args)
                               :cash-qty    (:cash_qty args)
                               :order-type  (or (:order_type args) "MKT")
                               :limit-price (:limit_price args)})]
                  (success (str "Order placed:\n" (pr-str result))))))})

(def cancel-order-tool
  {:name "cancel_order"
   :description "Cancel a pending order by order ID."
   :inputSchema {:type "object"
                 :properties {:order_id {:type "integer" :description "Order ID to cancel"}}
                 :required [:order_id]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(success (pr-str (conn/cancel-order! (:order_id args))))))})

(def search-contracts-tool
  {:name "search_contracts"
   :description "Search for contracts matching a symbol pattern. Returns matching instruments across exchanges."
   :inputSchema {:type "object"
                 :properties {:pattern {:type "string" :description "Symbol search pattern (e.g. 'AAPL', 'MSFT')"}}
                 :required [:pattern]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [results (conn/await-response (conn/search-contracts (:pattern args)))]
                  (if (:error results)
                    (failure (:error results))
                    (success (str/join "\n"
                                       (map (fn [{:keys [symbol sec-type currency primary-exch]}]
                                              (str symbol " | " sec-type " | " currency " | " primary-exch))
                                            results)))))))})

(def contract-details-tool
  {:name "get_contract_details"
   :description "Get full details for a specific contract (long name, industry, category, tick size, etc.)."
   :inputSchema {:type "object"
                 :properties {:symbol   {:type "string" :description "Ticker symbol"}
                              :sec_type {:type "string" :description "STK, OPT, FUT, CASH (default STK)"}
                              :exchange {:type "string" :description "Exchange (default SMART)"}
                              :currency {:type "string" :description "Currency (default USD)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [details (conn/await-response
                               (conn/request-contract-details
                                {:symbol   (:symbol args)
                                 :sec-type (or (:sec_type args) "STK")
                                 :exchange (or (:exchange args) "SMART")
                                 :currency (or (:currency args) "USD")}))]
                  (if (:error details)
                    (failure (:error details))
                    (success (str/join "\n\n"
                                       (map (fn [d]
                                              (str/join "\n"
                                                        (keep (fn [[k v]]
                                                                (when v (str (name k) ": " v)))
                                                              d)))
                                            details)))))))})

;; ---------------------------------------------------------------------------
;; Scanner tools

(def scanner-tool
  {:name "market_scanner"
   :description "Run a market scanner to find stocks matching criteria. Scan codes: TOP_PERC_GAIN, TOP_PERC_LOSE, MOST_ACTIVE, HOT_BY_VOLUME, HOT_BY_PRICE, HIGH_OPT_IMP_VOLAT, HIGH_DIVIDEND_YIELD, HIGH_PE_RATIO, LOW_PE_RATIO, HIGH_RETURN_ON_EQUITY, HIGH_VS_52W_HL, LOW_VS_52W_HL, TOP_TRADE_COUNT, TOP_VOLUME_RATE, MOST_ACTIVE_USD. Locations: STK.US.MAJOR, STK.US, STK.EU."
   :inputSchema {:type "object"
                 :properties {:scan_code       {:type "string" :description "Scan code (e.g. TOP_PERC_GAIN)"}
                              :location        {:type "string" :description "Market (default STK.US.MAJOR)"}
                              :num_rows        {:type "integer" :description "Number of results (default 20)"}
                              :above_price     {:type "number" :description "Min price filter"}
                              :below_price     {:type "number" :description "Max price filter"}
                              :above_volume    {:type "integer" :description "Min volume filter"}
                              :market_cap_above {:type "number" :description "Min market cap"}
                              :stock_type      {:type "string" :description "ALL, CORP, ETF, ADR, REIT (default ALL)"}}
                 :required [:scan_code]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [results (conn/await-response
                               (conn/request-scanner
                                {:scan-code       (:scan_code args)
                                 :location        (or (:location args) "STK.US.MAJOR")
                                 :num-rows        (or (:num_rows args) 20)
                                 :above-price     (:above_price args)
                                 :below-price     (:below_price args)
                                 :above-volume    (:above_volume args)
                                 :market-cap-above (:market_cap_above args)
                                 :stock-type      (or (:stock_type args) "ALL")})
                               :timeout 30000)]
                  (if (:error results)
                    (failure (:error results))
                    (if (empty? results)
                      (success "No results.")
                      (success (str/join "\n"
                                         (map (fn [{:keys [rank symbol long-name exchange projection]}]
                                                (str (inc rank) ". " symbol
                                                     (when long-name (str " — " long-name))
                                                     (when projection (str " | " projection))))
                                              results))))))))})

;; ---------------------------------------------------------------------------
;; News tools

(def news-providers-tool
  {:name "get_news_providers"
   :description "List available news providers on IBKR."
   :inputSchema {:type "object" :properties {}}
   :tool-fn (fn [_ctx _args]
              (safe-call
               #(let [providers (conn/await-response (conn/request-news-providers))]
                  (if (:error providers)
                    (failure (:error providers))
                    (success (str/join "\n" (map (fn [{:keys [code name]}]
                                                  (str code " — " name))
                                                providers)))))))})

(def news-headlines-tool
  {:name "get_news_headlines"
   :description "Get recent news headlines for a stock. Requires the contract's conId (get it via get_contract_details first)."
   :inputSchema {:type "object"
                 :properties {:con_id         {:type "integer" :description "Contract ID of the stock"}
                              :provider_codes {:type "string" :description "Comma-separated provider codes (default: BRFG,BRFUPDN,DJNL)"}
                              :total          {:type "integer" :description "Max headlines (default 30)"}}
                 :required [:con_id]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [headlines (conn/await-response
                                 (conn/request-historical-news
                                  {:con-id         (:con_id args)
                                   :provider-codes (or (:provider_codes args) "BRFG,BRFUPDN,DJNL")
                                   :total          (or (:total args) 30)}))]
                  (if (:error headlines)
                    (failure (:error headlines))
                    (if (empty? headlines)
                      (success "No news found.")
                      (success (str/join "\n"
                                         (map (fn [{:keys [time provider headline]}]
                                                (str time " [" provider "] " headline))
                                              headlines))))))))})

(def news-article-tool
  {:name "get_news_article"
   :description "Get the full text of a news article by provider code and article ID (from get_news_headlines)."
   :inputSchema {:type "object"
                 :properties {:provider_code {:type "string" :description "Provider code (e.g. BRFG)"}
                              :article_id    {:type "string" :description "Article ID (e.g. BRFG$04507322)"}}
                 :required [:provider_code :article_id]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [article (conn/await-response
                               (conn/request-news-article (:provider_code args) (:article_id args)))]
                  (if (:error article)
                    (failure (:error article))
                    (success (:text article))))))})

;; ---------------------------------------------------------------------------
;; Options chain tool

(def option-chain-tool
  {:name "get_option_chain"
   :description "Get available option expirations and strikes for an underlying stock. Requires con_id (get it via get_contract_details). Returns expirations and strikes per exchange."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Underlying symbol"}
                              :con_id {:type "integer" :description "Contract ID of the underlying"}}
                 :required [:symbol :con_id]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [chains (conn/await-response
                              (conn/request-option-chain
                               {:symbol (:symbol args) :con-id (:con_id args)}))]
                  (if (:error chains)
                    (failure (:error chains))
                    (success (str/join "\n\n"
                                       (map (fn [{:keys [exchange multiplier expirations strikes]}]
                                              (str "Exchange: " exchange " (x" multiplier ")\n"
                                                   "Expirations: " (str/join ", " (take 10 expirations))
                                                   (when (> (count expirations) 10) (str " ... (" (count expirations) " total)"))
                                                   "\nStrikes: " (str/join ", " (map (fn [s] (format "%.1f" s)) (take 20 strikes)))
                                                   (when (> (count strikes) 20) (str " ... (" (count strikes) " total)"))))
                                            chains)))))))})

;; ---------------------------------------------------------------------------
;; What-if order tool

(def what-if-tool
  {:name "check_order_impact"
   :description "Check the margin impact and estimated commission of an order WITHOUT actually placing it. Use this before placing real orders."
   :inputSchema {:type "object"
                 :properties {:symbol      {:type "string" :description "Ticker symbol"}
                              :sec_type    {:type "string" :description "STK, OPT, FUT (default STK)"}
                              :action      {:type "string" :description "BUY or SELL"}
                              :quantity    {:type "number" :description "Number of shares/contracts"}
                              :order_type  {:type "string" :description "MKT, LMT (default MKT)"}
                              :limit_price {:type "number" :description "Limit price (for LMT orders)"}
                              :exchange    {:type "string" :description "Exchange (default SMART)"}
                              :currency    {:type "string" :description "Currency (default USD)"}}
                 :required [:symbol :action :quantity]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [result (conn/await-response
                              (conn/what-if-order!
                               {:symbol   (:symbol args)
                                :sec-type (or (:sec_type args) "STK")
                                :exchange (or (:exchange args) "SMART")
                                :currency (or (:currency args) "USD")}
                               {:action      (:action args)
                                :quantity    (:quantity args)
                                :order-type  (or (:order_type args) "MKT")
                                :limit-price (:limit_price args)})
                              :timeout 15000)]
                  (if (:error result)
                    (failure (:error result))
                    (success (str/join "\n"
                                       [(str "Symbol: " (:symbol result))
                                        (str "Init Margin: " (:init-margin result))
                                        (str "Maint Margin: " (:maint-margin result))
                                        (str "Equity w/ Loan: " (:equity-with-loan result))
                                        (str "Commission: " (:commission result) " " (:commission-ccy result))
                                        (when (:warning result) (str "Warning: " (:warning result)))]))))))})

;; ---------------------------------------------------------------------------
;; WSH tools

(def wsh-events-tool
  {:name "get_calendar_events"
   :description "Get Wall Street Horizon calendar events (earnings, analyst meetings, dividends, etc.) for a contract. Requires con_id. Needs WSH subscription."
   :inputSchema {:type "object"
                 :properties {:con_id {:type "integer" :description "Contract ID"}
                              :start  {:type "string" :description "Start date YYYYMMDD"}
                              :end    {:type "string" :description "End date YYYYMMDD"}
                              :limit  {:type "integer" :description "Max events (default 50)"}}
                 :required [:con_id]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [data (conn/await-response
                            (conn/request-wsh-events
                             {:con-id (:con_id args)
                              :start  (:start args)
                              :end    (:end args)
                              :limit  (or (:limit args) 50)})
                            :timeout 15000)]
                  (if (:error data)
                    (failure (:error data))
                    (success data)))))})

;; ---------------------------------------------------------------------------
;; Yahoo Finance tools

(def company-fundamentals-tool
  {:name "get_company_fundamentals"
   :description "Get key fundamental data from Yahoo Finance for a stock: valuation (P/E, forward P/E, PEG, P/S, P/B, EV/EBITDA), profitability (margins, ROE, ROA), growth (revenue growth, earnings growth), financial health (debt/equity, current ratio), and trading data (beta, 52w high/low, dividend yield). Does NOT require IBKR connection."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Ticker symbol (e.g. AAPL, MSFT, NVDA)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(success (yahoo/fetch-fundamentals (:symbol args)))))})

(def company-profile-tool
  {:name "get_company_profile"
   :description "Get company profile from Yahoo Finance: name, sector, industry, employees, website, CEO, and business summary. Does NOT require IBKR connection."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Ticker symbol (e.g. AAPL, MSFT, NVDA)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(success (yahoo/fetch-profile (:symbol args)))))})

(def insider-trading-tool
  {:name "get_insider_trading"
   :description "Get recent insider transactions (buys, sells, gifts) for a stock from Yahoo Finance. Shows filer name, relation, date, shares, and value. Does NOT require IBKR connection."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Ticker symbol (e.g. AAPL, MSFT, NVDA)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(success (yahoo/fetch-insider-trading (:symbol args)))))})

(def institutional-holders-tool
  {:name "get_institutional_holders"
   :description "Get top institutional holders (13F data) for a stock from Yahoo Finance. Shows ownership breakdown (insider vs institutional %), top 15 holders with shares, value, and position changes. Does NOT require IBKR connection."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Ticker symbol (e.g. AAPL, MSFT, NVDA)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(success (yahoo/fetch-institutional-holders (:symbol args)))))})

(def analyst-estimates-tool
  {:name "get_analyst_estimates"
   :description "Get analyst estimates, recommendations, price targets, and recent upgrades/downgrades from Yahoo Finance. Includes EPS/revenue estimates for next 4 periods, buy/hold/sell consensus, and next earnings date. Does NOT require IBKR connection."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Ticker symbol (e.g. AAPL, MSFT, NVDA)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(success (yahoo/fetch-analyst-estimates (:symbol args)))))})

(def financial-statements-tool
  {:name "get_financial_statements"
   :description "Get income statement, balance sheet, and cash flow statement from Yahoo Finance. Supports annual or quarterly periods. Does NOT require IBKR connection."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Ticker symbol (e.g. AAPL, MSFT, NVDA)"}
                              :period {:type "string" :description "Period: 'annual' or 'quarter' (default: quarter)" :enum ["annual" "quarter"]}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(let [period (or (:period args) "quarter")]
                  (success (yahoo/fetch-financial-statements (:symbol args) :period period)))))})

(def earnings-calendar-tool
  {:name "get_earnings_calendar"
   :description "Get next earnings date, EPS/revenue estimates, and dividend dates for a stock from Yahoo Finance. Does NOT require IBKR connection."
   :inputSchema {:type "object"
                 :properties {:symbol {:type "string" :description "Ticker symbol (e.g. AAPL, MSFT, NVDA)"}}
                 :required [:symbol]}
   :tool-fn (fn [_ctx args]
              (safe-call
               #(success (yahoo/fetch-earnings-calendar (:symbol args)))))})

;; ---------------------------------------------------------------------------
;; All tools

(def all-tools
  [connect-tool
   disconnect-tool
   status-tool
   quote-tool
   fundamental-ratios-tool
   fundamentals-tool
   historical-data-tool
   positions-tool
   account-summary-tool
   open-orders-tool
   place-order-tool
   cancel-order-tool
   search-contracts-tool
   contract-details-tool
   scanner-tool
   news-providers-tool
   news-headlines-tool
   news-article-tool
   option-chain-tool
   what-if-tool
   wsh-events-tool
   company-fundamentals-tool
   company-profile-tool
   insider-trading-tool
   institutional-holders-tool
   analyst-estimates-tool
   financial-statements-tool
   earnings-calendar-tool])
