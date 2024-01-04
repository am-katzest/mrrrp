(ns mrrrp.message-handler
  (:require [mrrrp.effects :as fx]
            [mrrrp.finite-state-meowshine :as fsm]
            [clojure.tools.logging :as log]
            [mrrrp.repliers :as repliers]
            [io.pedestal.interceptor :as int]
            [io.pedestal.interceptor.chain :as chain]
            [mrrrp.gayboy :as g]))

;; input context map:
#_{:event-data  {}                      ; what discord gives
   :config {}
   :state {}}                           ; modifications are preserved

(def terminators [(fn [context] (not-empty (:fx context)))
                  (fn [context] (:stop context))])

(def fsm-repliers
  {:enter
   (fn [context]
     (let [state (-> context :state :fsm-states)
           {:keys [state fx]} (fsm/run-event-through state (:message context))
           context' (assoc-in context [:state :fsm-states] state)]
       (cond-> context'
         fx (assoc :fx fx))))})

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

(def postprocess-replies-interceptor
  {:leave
   (fn [context]
     (update context :fx (fn [fx]
                           (map (partial add-fx-context (-> context :message :channel)) fx))))})

(def update-blacklist-interceptor
  {:enter
   (fn [{:keys [message] :as context}]
     (let [stop-ctx (assoc context :stop true)]
      (case (:content message)
        "start meowing" (update-in stop-ctx [:state :blacklist] disj (:channel message))
        "stop meowing" (update-in stop-ctx [:state :blacklist] conj (:channel message))
        context)))})

(def apply-blacklist-interceptor
  {:enter
   (fn [context]
     (if (contains? (-> context :state :blacklist) (-> context :message :channel))
       (assoc context :stop true)
       context))})

(def ignore-self-interceptor
  {:enter
   (fn [context]
     (if (= (-> context :message :author) (-> context :config :bot-id))
       (assoc context :stop true)
       context))})



(def interceptors (->> [unpack-message
                        postprocess-replies-interceptor
                        ignore-self-interceptor
                        update-blacklist-interceptor
                        apply-blacklist-interceptor
                        g/update-gayboy-interceptor
                        g/ignore-gayboy-interceptor
                        fsm-repliers]
                       (map int/interceptor)))


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

(def initial-state {:blacklist #{}
                    :gayboy-channels #{}
                    :fsm-states repliers/repliers})
