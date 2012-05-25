(ns noir.cljs.client.watcher
  (:require [fetch.core :as fetch]
            [crate.core :as crate]
            [cljs.reader :as reader]
            [jayq.util :as util])
  (:use [jayq.core :only [$ append delegate data add-class remove-class find]]
        [crate.element :only [link-to]])
  (:use-macros [crate.def-macros :only [defpartial]]))

(defn wait [ms func]
  (js* "setTimeout(~{func}, ~{ms})"))

(def $body ($ :body))
(def callbacks (atom []))
(def cur-mode (atom :interactive))

(defn css-poll []
  (wait 100 #(fetch/xhr [:get "/noir-cljs-css-any-changes"] {}
                        (fn [data]
                          (when (= "true" data)
                            (js* "(function()
                                    {var h,a,f;a=document.getElementsByTagName('link');for(h=0;h<a.length;h++){f=a[h];if(f.rel.toLowerCase().match(/stylesheet/)&&f.href&&f.href.indexOf('data:')!=0){var g=f.href.replace(/(&|%5C?)forceReload=\\d+/,'');f.href=g+(g.match(/\\?/)?'&':'?')+'forceReload='+(new Date().valueOf())}}})()")
                            (util/log "data-changed"))
                          (when (= @cur-mode :interactive)
                            (css-poll))))))

(defn poll []
  (wait 100 #(fetch/xhr [:get "/noir-cljs-get-updated"] {}
                        (fn [data]
                          (when (and data
                                     (not= data ""))
                            (try
                              (js/eval data)
                              (catch js/ReferenceError e
                                (util/log (str "ReferenceError: " (. e -message))))
                              (catch js/TypeError e
                                (util/log (str "TypeError: " (. e -message))))
                              (catch js/Error e
                                (util/log (str "Error: " (. e -message))))
                            )
                            (doseq [cur @callbacks]
                              (try
                                (cur data)
                                (catch js/ReferenceError e
                                  (util/log (str "ReferenceError: " (. e -message))))
                                (catch js/TypeError e
                                  (util/log (str "TypeError: " (. e -message))))
                                (catch js/Error e
                                  (util/log (str "Error: " (. e -message))))
                              )
                            ))
                          (when (= @cur-mode :interactive)
                            (poll))))))

(defn on-update [func]
  (swap! callbacks conj func))

(defn set-mode [m]
  (fetch/xhr [:post "/noir-cljs-mode"] {:m m}
             (fn []
               (reset! cur-mode m)
               (when (= m :interactive)
                 (poll)))))

(defn get-mode [callback]
  (fetch/xhr [:get "/noir-cljs-mode"] {}
             (fn [x]
               (callback (reader/read-string x)))))

(def buttons [{:mode :advanced :label "A"}
              {:mode :simple :label "S"}
              {:mode :interactive :label "I"}])

(defpartial selector-button [{:keys [mode label]} & [m]]
  (let [klass (str "noir-cljs-button " (when (= mode m)
                                         "active"))]
    [:li (link-to {:class klass :data-mode mode} "#" label)]))

(defpartial selector [m]
  [:div
   [:style {:type "text/css"}
    "#noir-cljs-selector {position:fixed; bottom:15px; right:30px; list-style:none; }
     #noir-cljs-selector li {}
     #noir-cljs-selector a { float:left; display:inline; text-decoration:none; line-height:1em; height:19px; padding:5px 10px; background:#77c; width:10px; border:1px solid #55c; text-align:center; border-radius:5px; margin-bottom:8px; color:#449; }
     #noir-cljs-selector a:hover {background:#99f;}
     #noir-cljs-selector .active {background:#7cc; border-color:#599;} "] 
   [:ul#noir-cljs-selector
    (map #(selector-button % m) buttons)]])

(defn setup-delegates
  []
  (delegate $body :.noir-cljs-button :click
            (fn [e]
              (.preventDefault e)
              (remove-class (find ($ selector) :.noir-cljs-button) :active)
              (this-as me
                (let [$me ($ me)
                      mode (data $me :mode)]
                  (add-class $me :active)
                  (set-mode mode))))))

(defn init []
  (get-mode (fn [m]
              (setup-delegates)
              (when (= m :interactive)
                (poll))
              (css-poll)
              (append $body (selector m)))))
