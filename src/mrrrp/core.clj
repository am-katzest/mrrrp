(ns mrrrp.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [mrrrp.meowperhonsion :as c]
            [better-cond.core :as b]
            [mrrrp.slowdown :as slow]
            [mrrrp.gayboy :as g]
            [mrrrp.finite-state-meowshin :as fsm]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]])
  (:gen-class))

(def state (atom nil))
(def bot-id (atom nil))

(def blacklist (atom #{}))

(defn- stop-meowing [id]
  (swap! blacklist conj id)
  (print "blacklist is now " @blacklist))
(defn- start-meowing [id]
  (swap! blacklist disj id)
  (print "blacklist is now " @blacklist))

(defn make-replier [channel-id]
  (let [send-msg #(discord-rest/create-message! (:rest @state) channel-id :content  (str %))]
    (fn [msg] (slow/add channel-id #(send-msg  msg)))))

(defn prepare-event [cid uid msg]
  {:cid cid
   :uid uid
   :msg msg
   :reply-fn (make-replier cid)})

(defmulti handle-event (fn [type _data] type))
(defmethod handle-event :default [_ _])
(defmethod handle-event :message-create
  [_ {:keys [channel-id content author]}]
  (b/cond
    (= content "stop meowing") (stop-meowing channel-id)
    (= content "start meowing") (start-meowing channel-id)
    :when (not (@blacklist channel-id))
    :when (not= @bot-id (:id author))
    :do (fsm/accept-message! (prepare-event channel-id (:id author) content))
    :do (g/maybe-update-gayboy-meowing-area (:id author) channel-id content)
    :when (or (g/not-gayboy (:id author)) (> 0.1 (rand)))
    :when  (not (and (g/gayboy-in-channel? channel-id)
                     (g/is-gayboy-able-to-handle-this-message? content)))
    :do (c/maybe-meow-back content (make-replier channel-id))))

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

(defn  kill-bot! [{:keys [rest gateway events] :as _state}]
  (discord-ws/disconnect-bot! gateway))

(defmacro exp->some [& exprs]
  `(try ~@exprs
        (catch Throwable ~(gensym) nil)))

(defn get-token [meowken]
  (->> (or
        meowken
        (exp->some (slurp "MEOWKEN"))
        (System/getenv "MEOWKEN")
        (do
          (println "enter token!")
          (read-line)))
       (remove #{\newline})
       (reduce str)))

(defn -main [& meowken]
  (reset! state (start-bot! (get-token (first meowken)) :guild-messages))
  (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
  (future (try
            (message-pump! (:events @state) handle-event)
            (finally (stop-bot! @state)))))

(comment
  (-main)
  (kill-bot! @state))
