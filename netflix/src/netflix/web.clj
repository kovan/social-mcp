(ns netflix.web
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [netflix.cdp :as cdp])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(def base-url "https://www.netflix.com")

(defn- clean [s]
  (some-> s (str/replace #"\s+" " ") str/trim))

(defn- encode-query [s]
  (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn- js-str [s]
  (json/write-str (str s)))

(defn- search-url [query]
  (str base-url "/search?q=" (encode-query query)))

(defn- genre-url [code]
  (str base-url "/browse/genre/" (encode-query code)))

(def tag-presets
  {"korean_content" {:description "Korean movies and TV shows"
                     :query "Korean movies and TV shows"}
   "korean_romance" {:description "Korean romantic shows and movies"
                     :query "Korean romance"}
   "korean_comedy" {:description "Korean comedies"
                    :query "Korean comedy"}
   "kdrama" {:description "Korean dramas / K-drama"
             :query "K-drama"}
   "indian_movies" {:description "Indian and Hindi movies"
                    :query "Hindi movies"}
   "bollywood" {:description "Bollywood movies and shows"
                :query "Bollywood movies"}
   "feel_good" {:description "Light, upbeat, feel-good titles"
                :query "feel good movies"}
   "romantic_comedies" {:description "Romantic comedies"
                        :code "5475"}
   "comedies" {:description "Comedies"
               :code "6548"}
   "dramas" {:description "Dramas"
             :code "5763"}
   "horror" {:description "Horror movies"
             :code "8711"}
   "documentaries" {:description "Documentaries"
                    :code "6839"}
   "action" {:description "Action and adventure"
             :code "1365"}
   "international_movies" {:description "International movies"
                           :code "78367"}
   "uhd" {:description "Ultra HD / UHD titles"
          :query "UHD"}
   "atmos" {:description "Dolby Atmos titles"
            :query "Atmos"}})

(defn list-tag-presets []
  (->> tag-presets
       (map (fn [[tag data]]
              (assoc data :tag tag)))
       (sort-by :tag)
       vec))

(def ^:private accept-cookie-js
  "(function(){
     const buttons=[...document.querySelectorAll('button')];
     const b=buttons.find(x=>/Aceptar|Accept|Agree|Permitir/i.test((x.innerText||x.textContent||'')+' '+(x.value||'')));
     if(b){b.click(); return true;}
     return false;
   })()")

(defn- choose-profile-js [profile-name]
  (str
   "(function(profileName){
     const txt=(document.body && document.body.innerText)||'';
     const onProfile=/profiles|ProfileGate|¿Quién está viendo|Who's watching/i.test(location.href+' '+txt);
     if(!onProfile) return {clicked:false, reason:'not_profile_page', url:location.href};
     const links=[...document.querySelectorAll('.profile-link, a[href*=\"/SwitchProfile\"], [data-uia*=\"profile\"] a, a.profile')];
     const label=e=>((e.innerText||e.textContent||e.getAttribute('aria-label')||'')+'').replace(/\\s+/g,' ').trim();
     const visible=links.filter(e=>e.offsetParent!==null);
     const wanted=(profileName||'').toLowerCase();
     const exact=visible.find(e=>label(e).toLowerCase()===wanted);
     const contains=visible.find(e=>label(e).toLowerCase().includes(wanted));
     const nonKids=visible.find(e=>!/infantil|kids/i.test(label(e)));
     const target=exact || contains || nonKids || visible[0];
     if(target){target.click(); return {clicked:true, profile:label(target), url:location.href};}
     return {clicked:false, reason:'no_profile_link', url:location.href, profiles:visible.map(label), text:txt.slice(0,500)};
   })(" (js-str profile-name) ")"))

(def ^:private login-state-js
  "(function(){
     const text=(document.body && document.body.innerText)||'';
     return {
       url: location.href,
       login: /\\/login|Iniciar sesión|Sign In|Email or phone number/i.test(location.href+' '+text),
       text: text.slice(0,700)
     };
   })()")

(defn- profile-state-js [profile-name]
  (str
   "(function(profileName){
      const wanted=(profileName||'').toLowerCase();
      const userInfo=window.netflix && netflix.reactContext && netflix.reactContext.models && netflix.reactContext.models.userInfo && netflix.reactContext.models.userInfo.data;
      const profiles=(window.netflix && netflix.falcorCache && netflix.falcorCache.profiles) || {};
      const summaries=Object.values(profiles).map(p=>p && p.summary && p.summary.value).filter(Boolean);
      const target=summaries.find(p=>(p.profileName||'').toLowerCase()===wanted) ||
                   summaries.find(p=>(p.profileName||'').toLowerCase().includes(wanted));
      return {
        current_name: userInfo && userInfo.name,
        current_guid: userInfo && userInfo.userGuid,
        current_is_kids: userInfo && userInfo.isKids,
        target_name: target && target.profileName,
        target_guid: target && target.guid,
        profiles: summaries.map(p=>({name:p.profileName, guid:p.guid, isKids:p.isKids}))
      };
   })(" (js-str profile-name) ")"))

(def ^:private collect-results-js
  "(function(){
     const clean=s=>(s||'').replace(/\\s+/g,' ').trim();
     const nodes=[...document.querySelectorAll('[data-uia=\"search-gallery-video-card\"], [data-uia=\"title-card\"], .title-card, a[href*=\"/watch/\"], a[href*=\"/title/\"], a[href*=\"jbv=\"], [role=\"link\"][aria-label]')];
     const seen=new Set(), out=[];
     for(const n0 of nodes){
       const n=n0.closest('[data-uia=\"title-card\"], .title-card, a') || n0;
       const a=n.matches && n.matches('a') ? n : n.querySelector('a[href*=\"/watch/\"],a[href*=\"/title/\"],a[href*=\"jbv=\"]');
       const img=n.querySelector && n.querySelector('img');
       let title=clean(n.getAttribute('aria-label') ||
                       n.getAttribute('title') ||
                       (a && (a.getAttribute('aria-label') || a.getAttribute('title'))) ||
                       (img && (img.alt || img.getAttribute('aria-label'))) ||
                       n.innerText);
       title=title.replace(/^Ver tráiler de /i,'').replace(/^Reproducir /i,'').replace(/^Play /i,'');
       let url=a ? a.href : (n.href || null);
       const key=(title+'|'+url).toLowerCase();
       if(title && !/^(Reproducir|Play|Más información|More info)$/i.test(title) && !seen.has(key)){
         seen.add(key);
         out.push({title:title, url:url});
       }
     }
     return out;
   })()")

(defn- default-profile-name [profile-name]
  (or (not-empty (clean profile-name))
      (not-empty (System/getenv "NETFLIX_PROFILE"))
      "Javier"))

(defn- ensure-profile! [conn profile-name]
  (let [profile-name (default-profile-name profile-name)
        state (cdp/eval! conn (profile-state-js profile-name))]
    (if (and (:target_guid state)
             (not= (:current_guid state) (:target_guid state)))
      (do
        (cdp/navigate! conn (str base-url "/SwitchProfile?tkn=" (:target_guid state)))
        (cdp/wait-until! conn "document.readyState === 'complete' && !!document.body" 15000)
        (Thread/sleep 2500)
        (assoc state :switched true))
      (assoc state :switched false))))

(defn- ensure-ready! [conn profile-name]
  (cdp/wait-until! conn "document.readyState === 'complete' && !!document.body" 15000)
  (cdp/eval! conn accept-cookie-js)
  (let [profile (cdp/eval! conn (choose-profile-js (default-profile-name profile-name)))]
    (when (:clicked profile)
      (Thread/sleep 3500)))
  (let [state (cdp/eval! conn login-state-js)]
    (when (:login state)
      (throw (ex-info "Netflix is asking for login. Log in to netflix.com in Chrome first, then retry."
                      {:url (:url state)}))))
  (ensure-profile! conn profile-name))

(defn- navigate-ready! [conn url profile-name]
  (cdp/navigate! conn url)
  (when (:switched (ensure-ready! conn profile-name))
    (cdp/navigate! conn url)
    (cdp/wait-until! conn "document.readyState === 'complete' && !!document.body" 15000))
  conn)

(defn- collect-from-url [url {:keys [max-results max_results profile-name profile_name]}]
  (let [limit (min 50 (max 1 (int (or max_results max-results 10))))
        profile-name (or profile-name profile_name)
        conn (cdp/init-page! (cdp/open-page!))]
    (try
      (navigate-ready! conn url profile-name)
      (Thread/sleep 1500)
      (cdp/eval! conn "window.scrollTo(0, Math.min(document.body.scrollHeight, 1800)); true")
      (Thread/sleep 1000)
      (->> (or (cdp/eval! conn collect-results-js) [])
           (take limit)
           vec)
      (finally
        (cdp/close! conn)))))

(defn search-titles
  ([query] (search-titles query {}))
  ([query opts]
   (collect-from-url (search-url query) opts)))

(defn browse-genre
  ([code] (browse-genre code {}))
  ([code opts]
   (collect-from-url (genre-url code) opts)))

(defn search-tag
  [{:keys [tag code query] :as opts}]
  (let [tag (some-> tag clean)
        preset (get tag-presets tag)
        code (some-> (or code (:code preset)) str clean)
        query (some-> (or query (:query preset) tag) clean)]
    (cond
      (seq code)
      {:tag tag
       :mode "genre"
       :code code
       :url (genre-url code)
       :results (browse-genre code opts)}

      (seq query)
      {:tag tag
       :mode "query"
       :query query
       :url (search-url query)
       :results (search-titles query opts)}

      :else
      (throw (ex-info "search_netflix_tag requires tag, query, or code" {})))))

(defn open-netflix
  ([] (open-netflix {}))
  ([{:keys [url] :or {url (str base-url "/login")}}]
   (let [conn (cdp/init-page! (cdp/open-page!))]
     (cdp/navigate! conn url)
     (cdp/eval! conn accept-cookie-js)
     (let [state (cdp/eval! conn login-state-js)]
       {:status "opened"
        :url (:url state)
        :login_required (:login state)
        :message "Use the opened Chrome window to log in to Netflix. The profile is persisted under ~/.cache/social-mcp/netflix-chrome."}))))

(defn- open-matching-card-js [title]
  (str
   "(function(q){
      const norm=s=>(s||'').toLowerCase().normalize('NFD').replace(/[\\u0300-\\u036f]/g,'').replace(/[^a-z0-9]+/g,' ').trim();
      const clean=s=>(s||'').replace(/\\s+/g,' ').trim();
      const wanted=norm(q);
      const nodes=[...document.querySelectorAll('[data-uia=\"search-gallery-video-card\"], [data-uia=\"title-card\"], .title-card, a[href*=\"/watch/\"], a[href*=\"/title/\"], a[href*=\"jbv=\"], [role=\"link\"][aria-label]')];
      const scored=nodes.map(n0=>{
        const n=n0.closest('[data-uia=\"title-card\"], .title-card, a') || n0;
        const a=n.matches && n.matches('a') ? n : n.querySelector('a[href*=\"/watch/\"],a[href*=\"/title/\"],a[href*=\"jbv=\"]');
        const img=n.querySelector && n.querySelector('img');
        const raw=clean(n.getAttribute('aria-label') ||
                        n.getAttribute('title') ||
                        (a && (a.getAttribute('aria-label') || a.getAttribute('title'))) ||
                        (img && (img.alt || img.getAttribute('aria-label'))) ||
                        n.innerText);
        const t=raw.replace(/^Ver tráiler de /i,'').replace(/^Reproducir /i,'').replace(/^Play /i,'');
        const nt=norm(t);
        let score=0;
        if(nt===wanted) score=100;
        else if(nt.includes(wanted)) score=80;
        else if(wanted.includes(nt) && nt.length>2) score=60;
        return {node:n, link:a, title:t, url:a && a.href, score:score};
      }).filter(x=>x.score>0).sort((a,b)=>b.score-a.score);
      const target=scored[0];
      if(!target) return {opened:false, reason:'no_matching_card'};
      const el=target.link || target.node;
      el.scrollIntoView({block:'center', inline:'center'});
      target.node.dispatchEvent(new MouseEvent('mouseover',{bubbles:true}));
      el.click();
      return {opened:true, title:target.title, url:target.url, score:target.score};
    })(" (js-str title) ")"))

(def ^:private click-my-list-js
  "(function(){
     const label=e=>((e.getAttribute('aria-label')||'')+' '+(e.innerText||e.textContent||'')).replace(/\\s+/g,' ').trim();
     const visible=e=>!!(e.offsetWidth || e.offsetHeight || e.getClientRects().length);
     const buttons=[...document.querySelectorAll('button, [role=\"button\"]')].filter(visible);
     const already=buttons.find(b=>/Eliminar de Mi lista|Quitar de Mi lista|Remove from My List|In My List|En mi lista/i.test(label(b)));
     if(already) return {status:'already_added', label:label(already), url:location.href};
     const add=buttons.find(b=>/Añadir a Mi lista|Agregar a Mi lista|Mi lista|Add to My List|My List/i.test(label(b)) &&
                               !/Eliminar|Quitar|Remove/i.test(label(b)));
     if(add){
       add.scrollIntoView({block:'center'});
       add.click();
       return {status:'clicked', label:label(add), url:location.href};
     }
     return {status:'not_found',
             url:location.href,
             labels:buttons.map(label).filter(Boolean).slice(0,80),
             text:((document.body&&document.body.innerText)||'').slice(0,1200)};
   })()")

(defn add-to-my-list
  ([title] (add-to-my-list title {}))
  ([title {:keys [dry-run dry_run profile-name profile_name]}]
   (let [dry-run? (boolean (or dry-run dry_run))
         conn (cdp/init-page! (cdp/open-page!))]
     (try
       (let [url (search-url title)]
         (navigate-ready! conn url (or profile-name profile_name)))
       (Thread/sleep 1500)
       (let [results (cdp/eval! conn collect-results-js)
             opened (cdp/eval! conn (open-matching-card-js title))]
         (if-not (:opened opened)
           {:title title
            :status "not_found"
            :results (->> (or results []) (take 8) vec)}
           (do
             (Thread/sleep 2500)
             (if dry-run?
               {:title title
                :matched_title (:title opened)
                :status "dry_run"
                :url (:url opened)}
               (let [clicked (cdp/eval! conn click-my-list-js)]
                 {:title title
                  :matched_title (:title opened)
                  :status (:status clicked)
                  :button_label (clean (:label clicked))
                  :url (or (:url clicked) (:url opened))
                  :debug (when (= "not_found" (:status clicked))
                           (select-keys clicked [:labels :text]))})))))
       (finally
         (cdp/close! conn))))))

(defn bulk-add-to-my-list
  [{:keys [titles dry-run dry_run profile-name profile_name]}]
  (let [dry-run? (boolean (or dry-run dry_run))
        profile-name (or profile-name profile_name)
        titles (->> titles (map clean) (remove str/blank?) distinct vec)]
    (mapv (fn [title]
            (try
              (add-to-my-list title {:dry-run dry-run?
                                     :profile-name profile-name})
              (catch Exception e
                {:title title
                 :status "error"
                 :error (.getMessage e)})))
          titles)))
