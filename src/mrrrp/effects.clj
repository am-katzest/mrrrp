(ns mrrrp.effects
  (:require [clojure.tools.logging :as log]))

(defmulti run-fx (fn [type & _] type))

(def ^:dynamic *reply-fn*)
(defmethod run-fx :reply
  [_ value]
  (log/info "replying")
  (*reply-fn* value))

(defn run-fxs [fxs replier]
  (log/info "executing fxs" fxs)
  (binding [*reply-fn* replier]
    (doseq [fx fxs]
      (apply run-fx fx))))
