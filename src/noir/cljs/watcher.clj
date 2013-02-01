(ns noir.cljs.watcher
  (:import [java.util Calendar]
           [java.text SimpleDateFormat])
  (:require [noir.cljs.compiler :as compiler]
            [cljs.closure :as cljs]
            [colorize.core :as c]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [clj-redis.client :as redis]
            [clojure.data.json :as json])
  (:use [watchtower.core :only [watcher get-files ignore-dotfiles
                                extensions rate file-filter on-change]]))

(def r (redis/init {:url (get (System/getenv) "REDISTOGO_URL" "redis://127.0.0.1:6379")}))

(def options (atom {}))
(def watched (atom {}))
(def diffs (ref []))
(def mode (atom :interactive))
(def build-dirs ["checkouts/" "resources/javascript"])
(def last-compile-message (atom nil))

(defn ts []
    (let [c (Calendar/getInstance)
          f (SimpleDateFormat. "HH:mm:ss")]
      (c/magenta (.format f (.getTime c)))))

(defn ->name [f]
  (.getPath f))

(defn ->ns [form]
  (let [ns-form (first (filter #(= 'ns (first %)) (rest form)))
        sym (first (filter symbol? (rest ns-form)))]
    (or sym 'cljs.user)))

(defn clean [form]
  (walk/postwalk (fn [x]
                   (if (symbol? x)
                     (let [sname (name x)
                           index (.indexOf sname "__")]
                       (if (> index -1)
                         (symbol (subs sname 0 (+ 2 index)))
                         x))
                     x))
                 form))

(defn ->cljs-file [f]
  (let [form (read (compiler/->form (slurp f)))]
    {:ns (->ns form)
     :form (clean form)}))

(defn handle-error [ag message]
  (println "Error during compile - " message)
  (redis/publish r "*" (json/json-str { :chat_message "cljs-noir"
                                        :updates (str "console.log('" message "');")
                                      }))
  (reset! last-compile-message message))

(defn handle-file-error [f e]
  (handle-error nil (str f " :: " (.getMessage e))))

(defn init-file [f]
  (try
  (let [neue (->cljs-file f)]
    (compiler/->cljs (:form neue) (:ns neue))
    (swap! watched assoc (->name f) neue))
    (catch Exception e (handle-file-error f e))))

(defn update-file [f]
  (try
  (let [old-form (set (rest (get-in @watched [(->name f) :form])))
        neue (->cljs-file f)
        neue-form (set (rest (:form neue)))
        diff (set/difference neue-form old-form)
        updated (filter diff (rest (:form neue)))]
    (swap! watched assoc (->name f) neue)
    (when (seq updated)
      (let [entry [(:ns neue) (list* 'do updated)]]
        (println (ts) (c/green ":: sending ::") updated)
        (dosync (alter diffs conj entry)))))
  (catch Exception e (handle-file-error f e))))

(defn compile-options [m]
  (merge {:output-dir "resources/public/cljs/"
          :output-to "resources/public/cljs/bootstrap.js"
          :src-dir "src/"
          :optimizations m
          :pretty-print true}
         (@options m)))

(def my-agent (agent "start state"))
(set-error-handler! my-agent #(handle-error %1 (str "known file..." (.getMessage %2))))
(set-error-mode! my-agent :continue)

(defn build [state m]
  (println "old state was " state)
  (println "starting build")
  (let [options (compile-options m)]
    (cljs/build (:src-dir options) options))
  (println "built!")
  "new state")

(defn fire-off-build [m]
  (println "firing off build in bg")
  (send-off my-agent build m)
  (println "fired off build it bg"))

(defmulti on-file-changed (fn [m _] m))

(defmethod on-file-changed :interactive [_ fs]
  ; reset compiler warnings here rather than at the start of the build as the build
  ; is started *after* the initial run of init-files on new-planner start up
  (reset! last-compile-message nil)
  (doseq [f fs]
    (if (.contains (->name f) ".cljs")
      (if-not (@watched (->name f))
        (init-file f)
        (update-file f))))
  (dosync
    (let [entries @diffs
          cljs (for [[nsp form] entries] (compiler/->cljs form nsp))
          js (string/join "\n" cljs)]
      (println js)
      (ref-set diffs [])
      (redis/publish r "*" (json/json-str { :chat_message "cljs-noir"
                                            :updates js
                                          }))))
  (fire-off-build :simple))

(defmethod on-file-changed :simple [_ fs]
  (println (ts) (c/cyan ":: Simple compile"))
  (fire-off-build :simple)
  (println (ts) (c/green ":: Done")))

(defmethod on-file-changed :advanced [_ fs]
  (println (ts) (c/cyan ":: Advanced compile"))
  (fire-off-build :advanced)
  (println (ts) (c/green ":: Done")))

(defn update-files [fs]
  (println (ts) (c/cyan ":: Files updated " @mode))
  (try
    (on-file-changed @mode fs)
    (catch Exception e
      (.printStackTrace e))))

(defn set-mode [m]
  (reset! mode m))

(defn current-mode []
  @mode)

(defn start [& [opts]]
  (let [src-dir (or (get-in opts [@mode :src-dir]) "src/")]
    (println (ts) (c/cyan ":: Using source dir: " src-dir))
    (reset! options (or opts {}))
    (doseq [f (get-files [src-dir]
                         #(every? (fn [func] (func %))
                                  [(extensions :cljs) ignore-dotfiles]))]
      (init-file f))
    (watcher (list* src-dir build-dirs)
             (rate 100)
             (file-filter (extensions :cljs :js))
             (file-filter ignore-dotfiles)
             (on-change update-files))
    ))
