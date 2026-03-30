(ns pikabu.proxy
  "Shared HTTP proxy configuration for all Pikabu requests.
   Set PIKABU_PROXY env var (e.g. http://1.2.3.4:8080) or call set-proxy! at runtime.")

(def ^:private proxy-url
  (atom (System/getenv "PIKABU_PROXY")))

(defn set-proxy!
  "Set the proxy URL at runtime. Pass nil to disable."
  [url]
  (reset! proxy-url url))

(defn get-proxy
  "Return current proxy URL or nil."
  []
  @proxy-url)

(defn curl-args
  "Prepend --proxy to a curl arg list if a proxy is configured.
   Usage: (curl-args [\"-sSL\" url]) => [\"curl\" \"--proxy\" \"http://...\" \"-sSL\" url]"
  [args]
  (let [base (into ["curl"] args)]
    (if-let [px @proxy-url]
      (into ["curl" "--proxy" px] args)
      base)))
