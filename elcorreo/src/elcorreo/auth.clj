(ns elcorreo.auth
  "Extract Vocento auth tokens from Chrome cookies."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(def ^:private user-data (atom nil))

(defn- cookie-value-script
  [cookie-name]
  (str "import browser_cookie3, json\n"
       "orig_get_password = browser_cookie3._LinuxPasswordManager.get_password\n"
       "def patched_get_password(self, os_crypt_name):\n"
       "    try:\n"
       "        return orig_get_password(self, os_crypt_name)\n"
       "    except Exception:\n"
       "        return browser_cookie3.CHROMIUM_DEFAULT_PASSWORD\n"
       "browser_cookie3._LinuxPasswordManager.get_password = patched_get_password\n"
       "cj = browser_cookie3.chrome(domain_name='.elcorreo.com')\n"
       "for c in cj:\n"
       "    if c.name == " (pr-str cookie-name) ":\n"
       "        print(c.value)\n"
       "        break\n"))

(defn- extract-cookie-value
  [cookie-name]
  (let [script (cookie-value-script cookie-name)
        pb (ProcessBuilder. ["python3" "-c" script])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)]
    (when-not (zero? exit)
      (throw (ex-info (str "Cookie extraction failed: " err) {:exit exit})))
    out))

(defn- extract-voc-uid []
  (let [out (extract-cookie-value "voc_uid")]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn- extract-user-info []
  (let [out (extract-cookie-value "vocuser_information")]
    (when (seq out)
      (json/read-str out :key-fn keyword))))

(defn get-user
  "Returns auth data needed for commenting. Caches after first call."
  []
  (or @user-data
      (let [voc-uid (extract-voc-uid)
            user-info (extract-user-info)
            identity (-> user-info :userIdentity)
            user (:user identity)]
        (when (and voc-uid user)
          (let [data {:uid (:uid voc-uid)
                      :uidSignature (:uidSignature voc-uid)
                      :signatureTimestamp (:signatureTimestamp voc-uid)
                      :email (:email user)
                      :firstName (:firstName user)
                      :lastName (:lastName user)
                      :nickName (:nickName voc-uid)
                      :userType (or (:userType identity) "registrado")
                      :photo ""
                      :link ""}]
            (reset! user-data data)
            data)))))
