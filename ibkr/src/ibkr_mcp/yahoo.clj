(ns ibkr-mcp.yahoo
  "Yahoo Finance data fetching via public JSON endpoints.
   Uses java.net.HttpURLConnection with cookie/crumb auth — no extra dependencies."
  (:require [jsonista.core :as j]
            [clojure.string :as str])
  (:import [java.net HttpURLConnection URL URLEncoder CookieHandler CookieManager CookiePolicy]
           [java.io BufferedReader InputStreamReader]
           [java.text SimpleDateFormat]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; HTTP + Cookie/Crumb auth
;;
;; Yahoo Finance requires:
;; 1. A global CookieManager (to receive consent cookies)
;; 2. A crumb token fetched from /v1/test/getcrumb
;; 3. The crumb appended to every quoteSummary request

(defonce ^:private _cookie-init
  (CookieHandler/setDefault (CookieManager. nil CookiePolicy/ACCEPT_ALL)))

(def ^:private user-agent
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36")

(def ^:private mapper
  (j/object-mapper {:decode-key-fn keyword}))

(defn- http-get
  "GET a URL and return the response body as a string.
   Throws on non-2xx status unless :allow-error? is true."
  [^String url & {:keys [accept allow-error?]
                  :or   {accept "*/*" allow-error? false}}]
  (let [conn (doto ^HttpURLConnection (.openConnection (URL. url))
               (.setRequestMethod "GET")
               (.setRequestProperty "User-Agent" user-agent)
               (.setRequestProperty "Accept" accept)
               (.setInstanceFollowRedirects true)
               (.setConnectTimeout 10000)
               (.setReadTimeout 15000))
        status (.getResponseCode conn)]
    (when (and (not allow-error?) (not= (quot status 100) 2))
      (throw (ex-info (str "HTTP " status " from Yahoo Finance")
                      {:status status :url url})))
    (let [stream (if (< status 400)
                   (.getInputStream conn)
                   (.getErrorStream conn))]
      (when stream
        (with-open [rdr (BufferedReader. (InputStreamReader. stream "UTF-8"))]
          (let [sb (StringBuilder.)]
            (loop []
              (let [line (.readLine rdr)]
                (when line
                  (.append sb line)
                  (recur))))
            (str sb)))))))

;; ---------------------------------------------------------------------------
;; Crumb management
;;
;; The crumb is valid as long as the cookies remain valid.  We cache it and
;; re-fetch on auth failure (HTTP 401/403).

(def ^:private crumb-state (atom {:crumb nil :fetched-at 0}))

(defn- fetch-crumb!
  "Hit fc.yahoo.com to set cookies, then fetch a fresh crumb token."
  []
  ;; Step 1: touch fc.yahoo.com to receive consent cookies (404 is expected)
  (http-get "https://fc.yahoo.com" :allow-error? true)
  ;; Step 2: fetch crumb
  (let [crumb (http-get "https://query2.finance.yahoo.com/v1/test/getcrumb"
                        :accept "text/plain")]
    (reset! crumb-state {:crumb crumb :fetched-at (System/currentTimeMillis)})
    crumb))

(defn- get-crumb
  "Return a cached crumb, or fetch a fresh one if stale (>30 min) or missing."
  []
  (let [{:keys [crumb fetched-at]} @crumb-state
        age-ms (- (System/currentTimeMillis) (long fetched-at))]
    (if (and crumb (< age-ms (* 30 60 1000)))
      crumb
      (fetch-crumb!))))

;; ---------------------------------------------------------------------------
;; Yahoo Finance quoteSummary API

(defn- quote-summary
  "Fetch quoteSummary for symbol with given modules.
   Returns the parsed :quoteSummary :result first element, or throws.
   Retries once with a fresh crumb on auth failure."
  ([symbol modules] (quote-summary symbol modules false))
  ([^String symbol modules retried?]
   (let [crumb   (get-crumb)
         mod-str (str/join "," (map name modules))
         url     (str "https://query2.finance.yahoo.com/v10/finance/quoteSummary/"
                      (URLEncoder/encode symbol "UTF-8")
                      "?modules=" (URLEncoder/encode mod-str "UTF-8")
                      "&crumb=" (URLEncoder/encode crumb "UTF-8"))
         body    (try
                   (http-get url :accept "application/json")
                   (catch clojure.lang.ExceptionInfo e
                     (let [status (:status (ex-data e))]
                       (if (and (not retried?) (#{401 403} status))
                         ;; Crumb expired — fetch a fresh one and retry
                         (do (reset! crumb-state {:crumb nil :fetched-at 0})
                             ::retry)
                         (throw e)))))]
     (if (= body ::retry)
       (quote-summary symbol modules true)
       (let [parsed (j/read-value body mapper)
             result (get-in parsed [:quoteSummary :result])]
         (when (or (nil? result) (empty? result))
           (let [err (get-in parsed [:quoteSummary :error :description])]
             (throw (ex-info (or err (str "No data for " symbol))
                             {:symbol symbol}))))
         (first result))))))

;; ---------------------------------------------------------------------------
;; Value extraction helpers
;;
;; Yahoo wraps numbers in {:raw 123.45 :fmt "123.45"} objects.
;; Some fields are plain numbers or strings.

(defn- yval
  "Extract a display value from a Yahoo data field.
   Prefers :fmt (formatted string), falls back to :raw, then the value itself."
  [v]
  (cond
    (nil? v)        "N/A"
    (map? v)        (or (:fmt v)
                        (some-> (:raw v) str)
                        "N/A")
    :else           (str v)))

;; ---------------------------------------------------------------------------
;; Public API

(defn fetch-fundamentals
  "Fetch key fundamental data for a symbol. Returns a formatted string."
  [symbol]
  (let [data (quote-summary symbol [:financialData :defaultKeyStatistics :summaryDetail])
        fd   (:financialData data)
        ks   (:defaultKeyStatistics data)
        sd   (:summaryDetail data)
        ;; Some fields live in both defaultKeyStatistics and summaryDetail;
        ;; summaryDetail tends to be more complete for trading/valuation stats.
        v    (fn [& maps-and-key]
               (let [k    (last maps-and-key)
                     maps (butlast maps-and-key)]
                 (yval (some #(let [v (get % k)]
                                (when (and v (not= v {}) (not= (:raw v) nil))
                                  v))
                             maps))))]
    (str/join
     "\n"
     [(str symbol " — Key Fundamentals")
      (str (apply str (repeat 40 "-")))
      ""
      "VALUATION"
      (str "  Market Cap:         " (v sd fd :marketCap))
      (str "  Enterprise Value:   " (v ks :enterpriseValue))
      (str "  Trailing P/E:       " (v sd ks :trailingPE))
      (str "  Forward P/E:        " (v sd ks :forwardPE))
      (str "  PEG Ratio:          " (v ks :pegRatio))
      (str "  Price/Sales:        " (v sd ks :priceToSalesTrailing12Months))
      (str "  Price/Book:         " (v ks :priceToBook))
      (str "  EV/EBITDA:          " (v ks :enterpriseToEbitda))
      (str "  EV/Revenue:         " (v ks :enterpriseToRevenue))
      ""
      "PROFITABILITY"
      (str "  Profit Margin:      " (v fd :profitMargins))
      (str "  Operating Margin:   " (v fd :operatingMargins))
      (str "  Gross Margin:       " (v fd :grossMargins))
      (str "  EBITDA Margin:      " (v fd :ebitdaMargins))
      (str "  ROE:                " (v fd :returnOnEquity))
      (str "  ROA:                " (v fd :returnOnAssets))
      ""
      "GROWTH & INCOME"
      (str "  Revenue:            " (v fd :totalRevenue))
      (str "  Revenue Growth:     " (v fd :revenueGrowth))
      (str "  Earnings Growth:    " (v fd :earningsGrowth))
      (str "  EBITDA:             " (v fd :ebitda))
      (str "  Free Cash Flow:     " (v fd :freeCashflow))
      (str "  Operating Cash Flow:" (v fd :operatingCashflow))
      ""
      "FINANCIAL HEALTH"
      (str "  Total Debt:         " (v fd :totalDebt))
      (str "  Total Cash:         " (v fd :totalCash))
      (str "  Debt/Equity:        " (v fd :debtToEquity))
      (str "  Current Ratio:      " (v fd :currentRatio))
      (str "  Quick Ratio:        " (v fd :quickRatio))
      ""
      "TRADING"
      (str "  Beta:               " (v sd ks :beta))
      (str "  52-Week High:       " (v sd ks :fiftyTwoWeekHigh))
      (str "  52-Week Low:        " (v sd ks :fiftyTwoWeekLow))
      (str "  50-Day MA:          " (v sd ks :fiftyDayAverage))
      (str "  200-Day MA:         " (v sd ks :twoHundredDayAverage))
      (str "  Dividend Yield:     " (v sd ks :dividendYield))
      (str "  Trailing Div Rate:  " (v sd ks :trailingAnnualDividendRate))
      (str "  Payout Ratio:       " (v sd ks :payoutRatio))
      ""
      "SHARES"
      (str "  Shares Outstanding: " (v ks :sharesOutstanding))
      (str "  Float:              " (v ks :floatShares))
      (str "  Short % of Float:   " (v ks :shortPercentOfFloat))
      (str "  Short Ratio:        " (v ks :shortRatio))])))

(defn fetch-insider-trading
  "Fetch recent insider transactions for a symbol. Returns a formatted string."
  [symbol]
  (let [data  (quote-summary symbol [:insiderTransactions])
        txns  (get-in data [:insiderTransactions :transactions])]
    (if (empty? txns)
      (str symbol " — No insider transactions found")
      (str/join
       "\n"
       (concat
        [(str symbol " — Insider Transactions (latest " (min (count txns) 20) ")")
         (str (apply str (repeat 60 "-")))
         ""]
        (mapcat (fn [t]
                  [(str "  " (yval (:startDate t)) "  " (:filerName t) " (" (:filerRelation t) ")")
                   (str "    " (or (:transactionText t) "N/A")
                        "  Shares: " (yval (:shares t))
                        (when-let [v (:value t)]
                          (when (and (:raw v) (pos? (:raw v)))
                            (str "  Value: $" (yval v)))))
                   ""])
                (take 20 txns)))))))

(defn fetch-institutional-holders
  "Fetch top institutional holders for a symbol. Returns a formatted string."
  [symbol]
  (let [data    (quote-summary symbol [:institutionOwnership :majorHoldersBreakdown])
        holders (get-in data [:institutionOwnership :ownershipList])
        mhb     (:majorHoldersBreakdown data)]
    (str/join
     "\n"
     (concat
      [(str symbol " — Institutional Ownership")
       (str (apply str (repeat 50 "-")))
       ""
       "BREAKDOWN"
       (str "  Insiders:                " (yval (:insidersPercentHeld mhb)))
       (str "  Institutions:            " (yval (:institutionsPercentHeld mhb)))
       (str "  Institutions (float):    " (yval (:institutionsFloatPercentHeld mhb)))
       (str "  # of Institutions:       " (yval (:institutionsCount mhb)))
       ""
       "TOP HOLDERS"]
      (mapcat (fn [h]
                [(str "  " (:organization h))
                 (str "    Held: " (yval (:pctHeld h))
                      "  Shares: " (yval (:position h))
                      "  Value: $" (yval (:value h))
                      "  Change: " (yval (:pctChange h)))
                 ""])
              (take 15 holders))))))

(defn fetch-analyst-estimates
  "Fetch analyst estimates and recommendations for a symbol. Returns a formatted string."
  [symbol]
  (let [data   (quote-summary symbol [:earningsTrend :recommendationTrend :upgradeDowngradeHistory :calendarEvents])
        trends (get-in data [:earningsTrend :trend])
        recs   (get-in data [:recommendationTrend :trend])
        grades (get-in data [:upgradeDowngradeHistory :history])
        cal    (:calendarEvents data)]
    (str/join
     "\n"
     (concat
      [(str symbol " — Analyst Estimates & Recommendations")
       (str (apply str (repeat 50 "-")))
       ""]
      ;; Earnings estimates
      ["EARNINGS ESTIMATES"]
      (mapcat (fn [t]
                (let [ee (:earningsEstimate t)
                      re (:revenueEstimate t)]
                  [(str "  Period: " (:period t) " (ending " (:endDate t) ")"
                        "  Growth: " (yval (:growth t)))
                   (str "    EPS — Avg: " (yval (:avg ee))
                        "  Low: " (yval (:low ee))
                        "  High: " (yval (:high ee))
                        "  YoY: " (yval (:yearAgoEps ee))
                        "  (#Analysts: " (yval (:numberOfAnalysts ee)) ")")
                   (str "    Rev — Avg: " (yval (:avg re))
                        "  Low: " (yval (:low re))
                        "  High: " (yval (:high re))
                        "  (#Analysts: " (yval (:numberOfAnalysts re)) ")")
                   ""]))
              (take 4 trends))
      ;; Recommendation consensus
      ["CONSENSUS RECOMMENDATIONS"]
      (mapcat (fn [r]
                [(str "  " (:period r)
                      "  StrongBuy: " (:strongBuy r)
                      "  Buy: " (:buy r)
                      "  Hold: " (:hold r)
                      "  Sell: " (:sell r)
                      "  StrongSell: " (:strongSell r))])
              (take 4 recs))
      [""]
      ;; Recent upgrades/downgrades
      ["RECENT UPGRADES/DOWNGRADES"]
      (mapcat (fn [g]
                [(str "  " (when-let [d (:epochGradeDate g)]
                        (let [ms (* (long d) 1000)]
                          (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (java.util.Date. ms))))
                      "  " (:firm g)
                      ": " (:fromGrade g) " → " (:toGrade g)
                      (when (:currentPriceTarget g)
                        (str "  PT: $" (:currentPriceTarget g))))])
              (take 10 grades))
      [""]
      ;; Next earnings
      ["NEXT EARNINGS"]
      [(str "  Date: " (yval (first (get-in cal [:earnings :earningsDate])))
            (when (get-in cal [:earnings :isEarningsDateEstimate]) " (estimate)"))
       (str "  EPS Est: " (yval (get-in cal [:earnings :earningsAverage]))
            "  (Low: " (yval (get-in cal [:earnings :earningsLow]))
            "  High: " (yval (get-in cal [:earnings :earningsHigh])) ")")
       (str "  Rev Est: " (yval (get-in cal [:earnings :revenueAverage])))]))))

(defn fetch-financial-statements
  "Fetch income statement, balance sheet, and cash flow (quarterly). Returns a formatted string."
  [symbol & {:keys [period] :or {period "quarter"}}]
  (let [quarterly? (= period "quarter")
        modules    (if quarterly?
                     [:incomeStatementHistoryQuarterly :balanceSheetHistoryQuarterly :cashflowStatementHistoryQuarterly]
                     [:incomeStatementHistory :balanceSheetHistory :cashflowStatementHistory])
        data       (quote-summary symbol modules)
        inc-key    (if quarterly? :incomeStatementHistoryQuarterly :incomeStatementHistory)
        bs-key     (if quarterly? :balanceSheetHistoryQuarterly :balanceSheetHistory)
        cf-key     (if quarterly? :cashflowStatementHistoryQuarterly :cashflowStatementHistory)
        inc-stmts  (get-in data [inc-key :incomeStatementHistory])
        bs-stmts   (get-in data [bs-key :balanceSheetStatements])
        cf-stmts   (get-in data [cf-key :cashflowStatements])
        fmt-stmt   (fn [label fields stmt]
                     (concat
                      [(str "  " (yval (:endDate stmt)))]
                      (map (fn [[k lbl]]
                             (str "    " lbl ": " (yval (get stmt k))))
                           fields)
                      [""]))]
    (str/join
     "\n"
     (concat
      [(str symbol " — Financial Statements (" (if quarterly? "Quarterly" "Annual") ")")
       (str (apply str (repeat 50 "-")))
       ""
       "INCOME STATEMENT"]
      (mapcat (partial fmt-stmt "Income"
                       [[:totalRevenue "Revenue"]
                        [:grossProfit "Gross Profit"]
                        [:operatingIncome "Operating Income"]
                        [:netIncome "Net Income"]
                        [:ebit "EBIT"]])
              (take 4 inc-stmts))
      ["BALANCE SHEET"]
      (mapcat (partial fmt-stmt "Balance"
                       [[:totalAssets "Total Assets"]
                        [:totalLiab "Total Liabilities"]
                        [:totalStockholderEquity "Stockholder Equity"]
                        [:cash "Cash"]
                        [:longTermDebt "Long-Term Debt"]])
              (take 4 bs-stmts))
      ["CASH FLOW"]
      (mapcat (partial fmt-stmt "CashFlow"
                       [[:totalCashFromOperatingActivities "Cash from Operations"]
                        [:capitalExpenditures "CapEx"]
                        [:totalCashflowsFinancing "Financing"]
                        [:issuanceOfStock "Stock Issuance"]
                        [:repurchaseOfStock "Stock Repurchase"]
                        [:dividendsPaid "Dividends Paid"]])
              (take 4 cf-stmts))))))

(defn fetch-earnings-calendar
  "Fetch upcoming earnings date and dividend info for a symbol. Returns a formatted string."
  [symbol]
  (let [data (quote-summary symbol [:calendarEvents])
        cal  (:calendarEvents data)
        earn (:earnings cal)]
    (str/join
     "\n"
     [(str symbol " — Earnings Calendar")
      (str (apply str (repeat 40 "-")))
      ""
      (str "Next Earnings Date:  " (yval (first (:earningsDate earn)))
           (when (:isEarningsDateEstimate earn) " (estimate)"))
      (str "Last Earnings Call:  " (yval (first (:earningsCallDate earn))))
      (str "EPS Estimate:        " (yval (:earningsAverage earn))
           "  (Low: " (yval (:earningsLow earn))
           "  High: " (yval (:earningsHigh earn)) ")")
      (str "Revenue Estimate:    " (yval (:revenueAverage earn)))
      ""
      (str "Ex-Dividend Date:    " (yval (:exDividendDate cal)))
      (str "Dividend Date:       " (yval (:dividendDate cal)))])))

(defn fetch-profile
  "Fetch company profile for a symbol. Returns a formatted string."
  [symbol]
  (let [data (quote-summary symbol [:assetProfile])
        ap   (:assetProfile data)]
    (when (nil? ap)
      (throw (ex-info (str "No profile data for " symbol) {:symbol symbol})))
    (let [summary   (or (:longBusinessSummary ap) "N/A")
          truncated (if (> (count summary) 500)
                      (str (subs summary 0 497) "...")
                      summary)]
      (str/join
       "\n"
       [(str symbol " — Company Profile")
        (str (apply str (repeat 40 "-")))
        ""
        (str "Sector:              " (or (:sector ap) "N/A"))
        (str "Industry:            " (or (:industry ap) "N/A"))
        (str "Country:             " (or (:country ap) "N/A"))
        (str "City:                " (or (:city ap) "N/A") ", " (or (:state ap) ""))
        (str "Website:             " (or (:website ap) "N/A"))
        (str "Full-Time Employees: " (or (:fullTimeEmployees ap) "N/A"))
        (str "CEO:                 "
             (let [officers (:companyOfficers ap)]
               (if (seq officers)
                 (let [ceo (or (first (filter #(some-> (:title %) (str/includes? "CEO")) officers))
                               (first officers))]
                   (str (:name ceo) " — " (:title ceo)))
                 "N/A")))
        ""
        "Business Summary:"
        truncated]))))
