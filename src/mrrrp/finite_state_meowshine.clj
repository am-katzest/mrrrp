(ns mrrrp.finite-state-meowshine
  (:require [statecharts.core :as fsm]))
;; output of these functions dechannele what state to use
(defn only-channel [{:keys [channel]}] channel)

(defn user-in-channel [{:keys [channel author]}] [channel author])

(defmulti apply-event (fn [_ r] (:type r)))

(defmethod apply-event :fsm
  [event {:keys [machine location extract-type states] :as x}]
  (let [loc (location event)
        type (try  (extract-type event)
                   (catch Throwable t
                     {:extract-type-failed-with t}))
        event' (assoc event :type type)
        state (or (states loc) (fsm/initialize machine))
        state' (try (fsm/transition machine state event')
                    (catch Throwable t  ;inelegant, change
                      (prn "state transition failed" {:old-state state :event event' :exception t})
                      state))
        state'' (dissoc state' :fx)]
    {:fx (:fx state')
     :state (assoc-in x [:states loc] state'')}))

(defmethod apply-event :function
  [event {:keys [function] :as x}]
  {:fx (function event)
   :state x})

(defn run-event-through [fsms event]
  (let [together (mapv (partial apply-event event) fsms)]
    {:state (mapv :state together)
     :fx (mapcat :fx together)}))

(defn make-fsm [machine location extract]
  {:type :fsm
   :machine machine
   :id (:id machine)
   :location location
   :extract-type extract
   :states {}})

(defn make-fn [f id]
  {:type :function
   :id id
   :function f})

(defn addfx
  "adds side effects to fsm, used with :actions"
  [& ks]
  (fsm/assign (fn [s & _]
                (assoc s :fx ks))))

(defn reply [text] (addfx [:reply text]))
