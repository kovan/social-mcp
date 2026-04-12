(ns hn.cookies
  "Extract cookies from Linux Chrome via browser_cookie3 (Python)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(defn- cookie-script
  [domain]
  (str
   "import browser_cookie3, json\n"
   "orig_get_password = browser_cookie3._LinuxPasswordManager.get_password\n"
   "def patched_get_password(self, os_crypt_name):\n"
   "    try:\n"
   "        return orig_get_password(self, os_crypt_name)\n"
   "    except Exception:\n"
   "        return browser_cookie3.CHROMIUM_DEFAULT_PASSWORD\n"
   "browser_cookie3._LinuxPasswordManager.get_password = patched_get_password\n"
   "cj = browser_cookie3.chrome(domain_name=" (pr-str domain) ")\n"
   "print(json.dumps([{"
   "'name':c.name,'value':c.value,'path':c.path,'host':c.domain"
   "} for c in cj]))"))

(defn get-cookies
  "Extract cookies for `domain` from Chrome's cookie store.
   Requires `pip install browser-cookie3`.
   Returns a seq of {:name :value :path :host} maps."
  [domain]
  (let [script (cookie-script domain)
        pb (ProcessBuilder. ["python3" "-c" script])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)]
    (when-not (zero? exit)
      (throw (ex-info (str "Cookie extraction failed: " err) {:exit exit})))
    (json/read-str out :key-fn keyword)))
