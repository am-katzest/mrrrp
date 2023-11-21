(ns mrrrp.message-handler
  (:require [mrrrp.effects :as fx]
            [mrrrp.finite-state-meowshine :as fsm]
            [clojure.tools.logging :as log]
            [better-cond.core :as b]
            [mrrrp.gayboy :as g]
            [mrrrp.slowdown :as slow]))

(def blacklist (atom #{}))

(defn- stop-meowing [id]
  (swap! blacklist conj id)
  (log/info "blacklist is now " @blacklist))

(defn- start-meowing [id]
  (swap! blacklist disj id)
  (log/info "blacklist is now " @blacklist))

(defn prepare-event [cid uid msg]
  {:cid cid
   :uid uid
   :msg msg})

(defn add-fx-context [channel fx]
  (case (first fx)
    :reply {:type :reply :channel channel :text (str (second fx))}
    fx))

(defn handle-message
  "takes state, config and message and returns updated state and fxs"
  [{:keys [bot-id]} state {:keys [channel-id content author]}]
  (b/cond
    (= content "stop meowing") (stop-meowing channel-id)
    (= content "start meowing") (start-meowing channel-id)
    :when (not (@blacklist channel-id))
    :when (not= bot-id (:id author))
    :do (g/maybe-update-gayboy-meowing-area (:id author) channel-id content)
    :when (or (g/not-gayboy (:id author)) (> 0.1 (rand)))
    :when  (not (and (g/gayboy-in-channel? channel-id)
                     (g/is-gayboy-able-to-handle-this-message? content)))
    [state
     (->>
      (prepare-event channel-id (:id author) content)
      fsm/accept-message!
      (map (partial add-fx-context channel-id)))]))
