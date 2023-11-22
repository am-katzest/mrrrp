(ns mrrrp.message-handler
  (:require [mrrrp.effects :as fx]
            [mrrrp.finite-state-meowshine :as fsm]
            [clojure.tools.logging :as log]
            [io.pedestal.interceptor :as int]
            [io.pedestal.interceptor.chain :as chain]
            [better-cond.core :as b]
            [mrrrp.gayboy :as g]
            [mrrrp.slowdown :as slow]))

;; input context map:
#_{:event-data  {}                      ; what discord gives
   :config {}
   :state {}}                           ; modifications are preserved


(def terminators [(fn [context] (not-empty (:fx context)))])

(def always-meow
  {:enter
   (fn [context]
     (assoc context :fx [[:reply "meow"]]))})

(def unpack-message
  {:enter
   (fn [{:keys [event] :as context}]
     (assoc context :message
            {:author (:id (:author event))
             :channel (:channel-id event)
             :content (:content event)}))})

(defn add-fx-context [channel fx]
  (case (first fx)
    :reply {:type :reply :channel channel :text (str (second fx))}
    fx))

(def postprocess-replies
  {:leave
   (fn [context]
     (update context :fx (fn [fx]
                           (map (partial add-fx-context (-> context :message :channel)) fx))))})

(def interceptors (->> [unpack-message
                        postprocess-replies
                        always-meow]
                       (map int/interceptor)))

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


(defn handle-message
  "takes state, config and message and returns updated state and fxs"
  [config state event]
  (chain/execute
   {::chain/terminators terminators
    :event event
    :state state
    :config config}
   interceptors))
;; (handle-message {} initial-state {})

#_(defn handle-message-old
    "takes state, config and message and returns updated state and fxs"
    [{:keys [bot-id]} {:keys [blacklist]  :as state} {:keys [channel-id content author]}]
    (b/cond
      :do (println "aaa-" blacklist)
      (= content "stop meowing") (stop-meowing channel-id)
      (= content "start meowing") (start-meowing channel-id)
      :when (not (blacklist channel-id))
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

(def initial-state {:blacklist #{}})
