(ns amazon.cookies
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(defn- cookie-script
  [domain]
  (str
   "import browser_cookie3, json\n"
   "browser_cookie3._LinuxPasswordManager.get_password = lambda self, name: browser_cookie3.CHROMIUM_DEFAULT_PASSWORD\n"
   "cj = browser_cookie3.chrome(domain_name=" (pr-str domain) ")\n"
   "print(json.dumps([{"
   "'name':c.name,'value':c.value,'path':c.path,'host':c.domain,"
   "'secure':c.secure,'expires':c.expires"
   "} for c in cj]))"))

(defn get-cookies
  ([] (get-cookies "amazon.es"))
  ([domain]
   (let [proc (.start (ProcessBuilder. ["python3" "-c" (cookie-script domain)]))
         out (str/trim (slurp (.getInputStream proc)))
         err (str/trim (slurp (.getErrorStream proc)))
         exit (.waitFor proc)]
     (when-not (zero? exit)
       (throw (ex-info (str "Cookie extraction failed: " err) {:exit exit})))
     (json/read-str out :key-fn keyword))))

(defn write-netscape-cookie-file!
  [file cookies]
  (spit file "# Netscape HTTP Cookie File\n")
  (doseq [{:keys [name value path host secure expires]} cookies]
    (when (and (seq name) (seq value) (seq host))
      (spit file
            (str host "\t"
                 (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                 (or path "/") "\t"
                 (if secure "TRUE" "FALSE") "\t"
                 (long (or expires 0)) "\t"
                 name "\t"
                 value "\n")
            :append true)))
  file)
