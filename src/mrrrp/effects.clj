(ns mrrrp.effects)
;; the dirty part

(defmulti run-fx (fn [type & _] type))

(def ^:dynamic *reply-fn*)
(defmethod run-fx :reply
  [_ value]
  (*reply-fn* value))

(defn run-fxs [fxs replier]
  (println "fxs" fxs)
  (binding [*reply-fn* replier]
    (doseq [fx fxs]
      (apply run-fx fx)))
  (println "finished"))
