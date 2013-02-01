(ns noir.cljs.client.watcher
  (:require [fetch.core :as fetch]
            [crate.core :as crate]
            [cljs.reader :as reader]
            [jayq.util :as util]
            [goog.net.BrowserChannel :as goog-browserchannel]
            [goog.events :as events]
            [goog.events.KeyCodes :as key-codes]
            [goog.events.KeyHandler :as key-handler]
            )
  (:use [jayq.core :only [$ append delegate data add-class remove-class find]]
        [crate.element :only [link-to]])
  (:use-macros [crate.def-macros :only [defpartial]]))

(defn wait [ms func]
  (js* "setTimeout(~{func}, ~{ms})"))

(def $body ($ :body))
(def callbacks (atom []))
(def cur-mode (atom :interactive))

(def channel (goog.net.BrowserChannel.))

(def handlers (atom []))

(defn on-push [func]
  (swap! handlers conj func))

(defn handler []
  (let [h (goog.net.BrowserChannel.Handler.)]
    (set! (.-channelOpened h)
          (fn [channel]))
    (set! (.-channelHandleArray h)
          (fn [x data]
            (let [msg (aget data "msg")]
              (doseq [cur @handlers]
                (try
                  (cur msg)
                  (catch js/Error e
                    (util/log e))))
              )))
    h))

(defn say [text]
  (.sendMap channel (doto (js-obj)
                      (aset "msg" text)) ))

(defn ^:export run []
  (events/listen js/window "unload" #(do
                                       (.disconnect channel ())
                                       (events/removeAll)))
  (doto (.. channel getChannelDebug getLogger)
      (.setLevel goog.debug.Logger.Level.OFF))
  (doto channel
    (.setHandler (handler))
    (.connect "/channel/test" "/channel/bind"))
  (on-push (fn [raw]
             (if (= (subs raw 0 14) "redis message " )
               (let [msg (JSON/parse (subs raw 14))
                     msgch (aget msg "chat_message")
                     data (aget msg "updates")]
                 (if (= msgch "cljs-noir")
                   (do
                     (eval-data data))))))))

(defn css-poll []
  (wait 500 #(fetch/xhr [:get "/noir-cljs-css-any-changes"] {}
                        (fn [data]
                          (when (= "true" data)
                            (js* "(function()
                                    {var h,a,f;a=document.getElementsByTagName('link');for(h=0;h<a.length;h++){f=a[h];if(f.rel.toLowerCase().match(/stylesheet/)&&f.href&&f.href.indexOf('data:')!=0){var g=f.href.replace(/(&|%5C?)forceReload=\\d+/,'');f.href=g+(g.match(/\\?/)?'&':'?')+'forceReload='+(new Date().valueOf())}}})()")
                            (util/log "data-changed"))
                          (when (= @cur-mode :interactive)
                            (css-poll))))))

(defn poll []
  (wait 500 #(fetch/xhr [:get "/noir-cljs-get-updated"] {}
                        (fn [data]
                          (eval-data data)
                          (when (= @cur-mode :interactive)
                            (poll))))))

(defn eval-data [data]
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
                            )))

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
                ; (poll)
                ; (css-poll)
                (run)
                )
              (append $body (selector m)))))
