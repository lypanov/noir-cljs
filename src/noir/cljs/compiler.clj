(ns noir.cljs.compiler
  (:require [cljs.compiler :as comp]
            [cljs.analyzer :as anal])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io StringReader)))

(defn lined-reader
  "Create a line preserving reader for line-aware code evaluation in a sandbox."
  [s]
  (let [rdr (StringReader. s)]
    (LineNumberingPushbackReader. rdr)))

(defn ->form [s]
  (lined-reader (str "(do " s ")")))

(defn find-changed [form line-nums]
  (let [form (read (->form form))
        lined (map (juxt (comp :line meta) identity) (rest form))
        grouped (partition 2 1 lined)]
    (reduce
      (fn [changed line]
        (let [line (inc line)
              _ (println line)
              found (first (filter (fn [[[l1] [l2] :as me]]
                                     (println me)
                                     (and (>= line (or l1 0))
                                          (< line l2)))
                                   grouped))]
          (if found
            (-> found
                first
                second)
            (-> grouped
                last
                second
                second))))
      []
      line-nums)))

(defn ->cljs [f & [nsp]]
  (binding [anal/*cljs-ns* (or nsp 'cljs.user)]
    (let [form (if (string? f)
                 (binding [*ns* (create-ns anal/*cljs-ns*)]
                   (read (->form f)))
                 f)
          env {:context :statement :locals {}}
          env (assoc env :ns (@anal/namespaces anal/*cljs-ns*))
          ast (anal/analyze env form)
          js (with-out-str (comp/emits ast))
          wrap-js (with-out-str (comp/emits (binding [anal/*cljs-warn-on-undeclared* false]
                                (anal/analyze env form))))]
      wrap-js)))

