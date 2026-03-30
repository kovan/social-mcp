(ns ibkr-mcp.server
  "Cantillon Trading System — MCP server.
   Exposes IBKR tools + Cantillon thesis context via stdio JSON-RPC."
  (:require [clojure.java.io :as io]
            [mcp-toolkit.server :as server]
            [mcp-toolkit.json-rpc :as json-rpc]
            [jsonista.core :as j]
            [ibkr-mcp.tools :as tools])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io OutputStreamWriter])
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Resources — Cantillon thesis context

(defn- load-doc [filename]
  (let [f (io/file "docs" filename)]
    (when (.exists f) (slurp f))))

(def thesis-resource
  {:uri "cantillon://thesis"
   :name "Cantillon Thesis & Strategy"
   :description "Project brief: Cantillon Effect thesis, target instruments, risk parameters, architecture"
   :mimeType "text/markdown; charset=UTF-8"
   :text (or (load-doc "cantillon_project_brief.md")
             "Project brief not found — place cantillon_project_brief.md in docs/")})

(def watchlist-resource
  {:uri "cantillon://watchlist"
   :name "Cantillon Watchlist"
   :description "Investment watchlist organized by Cantillon proximity: first-order recipients (semis, hyperscalers, energy, defense), second-order (construction, power equipment, materials), EM rotation play"
   :mimeType "text/markdown; charset=UTF-8"
   :text (or (load-doc "cantillon_watchlist.md")
             "Watchlist not found — place cantillon_watchlist.md in docs/")})

;; ---------------------------------------------------------------------------
;; Prompts — analysis templates

(def cantillon-analysis-prompt
  {:name "cantillon_analysis"
   :description "Analyze a stock or sector through the Cantillon Effect lens"
   :arguments [{:name "symbol"
                :description "Ticker symbol or sector to analyze"
                :required true}]
   :prompt-fn (fn [_context {:keys [symbol]}]
                {:description (str "Cantillon Effect analysis for " symbol)
                 :messages [{:role "user"
                             :content {:type "text"
                                       :text (str "Analyze " symbol " through the Cantillon Effect lens.\n\n"
                                                  "Consider:\n"
                                                  "1. How close is this company/sector to new money creation (Fed purchases, fiscal spending, AI capex)?\n"
                                                  "2. Is it a first-order recipient (direct beneficiary) or second-order (downstream, 6-18mo lag)?\n"
                                                  "3. What are the key catalysts? (Fed meetings, earnings, government contract announcements)\n"
                                                  "4. Current valuation — has the Cantillon advantage already been priced in?\n"
                                                  "5. Risk factors specific to this name\n\n"
                                                  "Use the available market data tools to pull current prices, fundamentals, and historical data.\n"
                                                  "Reference the Cantillon watchlist resource for context on the overall thesis.\n\n"
                                                  "Conclude with: BUY / HOLD / AVOID and suggested position sizing (conservative).")}}]})})

(def fed-signal-prompt
  {:name "fed_signal_check"
   :description "Check for Cantillon signals from recent Fed activity"
   :arguments []
   :prompt-fn (fn [_context _args]
                {:description "Fed signal check for Cantillon thesis"
                 :messages [{:role "user"
                             :content {:type "text"
                                       :text (str "Check for Cantillon Effect signals:\n\n"
                                                  "1. Pull current market data for the watchlist symbols\n"
                                                  "2. Look for unusual moves in first-order recipients (NVDA, AMD, AVGO, CEG, NEE)\n"
                                                  "3. Compare performance of first-order vs second-order names\n"
                                                  "4. Flag any names showing relative strength or weakness\n"
                                                  "5. Note the next Fed meeting date and any upcoming catalysts\n\n"
                                                  "Summarize: Which positions look actionable right now based on the Cantillon thesis?")}}]})})

;; ---------------------------------------------------------------------------
;; Session

(def session
  (atom (server/create-session {:tools tools/all-tools
                                :resources [thesis-resource watchlist-resource]
                                :prompts [cantillon-analysis-prompt
                                          fed-signal-prompt]})))

;; ---------------------------------------------------------------------------
;; stdio transport

(defn- listen [context ^LineNumberingPushbackReader reader]
  (let [mapper (j/object-mapper {:decode-key-fn keyword})]
    (loop []
      (when-some [line (.readLine reader)]
        (let [message (try
                        (j/read-value line mapper)
                        (catch Exception _
                          ((:send-message context) json-rpc/parse-error-response)
                          nil))]
          (when message
            (json-rpc/handle-message context message))
          (recur))))))

(defn- make-context
  "Build MCP context, capturing the current *out* for JSON-RPC responses."
  []
  {:session session
   :send-message (let [^OutputStreamWriter writer *out*
                       mapper (j/object-mapper {:encode-key-fn name})]
                   (fn [message]
                     (.write writer (j/write-value-as-string message mapper))
                     (.write writer "\n")
                     (.flush writer)))})

;; ---------------------------------------------------------------------------
;; Entry point

(defn main
  "Start the MCP server. Reads JSON-RPC from stdin, writes to stdout.
   Optional :nrepl-port for live development."
  ([] (main {}))
  ([opts]
   (let [nrepl-server (when-let [port (:nrepl-port opts)]
                        (require 'nrepl.server)
                        ((resolve 'nrepl.server/start-server)
                         {:bind "127.0.0.1" :port port}))]
     (try
       (let [ctx (make-context)]
         (binding [*out* *err*]
           (println "Cantillon MCP server starting..."))
         (listen ctx *in*))
       (finally
         (when nrepl-server
           ((resolve 'nrepl.server/stop-server) nrepl-server)))))))

(defn -main
  "CLI entry point for -M mode."
  [& _args]
  (main {}))
