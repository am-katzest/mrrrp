(ns mrrrp.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [mrrrp.meowperhonsion :as c]
            [better-cond.core :as b]
            [mrrrp.slowdown :as slow]
            [mrrrp.gayboy :as g]
            [mrrrp.finite-state-meowshine :as fsm]
            [mrrrp.effects :as fx]
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

(defn add-usage-meter [f]
  (let [count (atom 0)
        f' (fn [& x]
             (swap! count inc)
             (apply f x))]
    [count f']))

(defn make-useable-only-once [f]
  (let [used (atom false)]
    (fn [& x]
      (when-not @used
        (reset! used true)
        (apply f x)))))

(defn prepare-event [cid uid msg]
  {:cid cid
   :uid uid
   :msg msg})

(defmulti handle-event (fn [type _data] type))
(defmethod handle-event :default [_ _])

(defmethod handle-event :message-create
  [_ {:keys [channel-id content author]}]
  (b/cond
    (= content "stop meowing") (stop-meowing channel-id)
    (= content "start meowing") (start-meowing channel-id)
    :when (not (@blacklist channel-id))
    :when (not= @bot-id (:id author))
    :let [replier (make-useable-only-once (make-replier channel-id))]
    :do (g/maybe-update-gayboy-meowing-area (:id author) channel-id content)
    :when (or (g/not-gayboy (:id author)) (> 0.1 (rand)))
    :let [fx (fsm/accept-message! (prepare-event channel-id (:id author) content))]
    (not-empty fx) (fx/run-fxs fx replier)
    ;when any of more advanced behaviours activated, don't just meow back
    :when  (not (and (g/gayboy-in-channel? channel-id)
                     (g/is-gayboy-able-to-handle-this-message? content)))
    :do (c/maybe-meow-back content replier)))

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
        (println "trying to get token from file...")
        (exp->some (slurp "MEOWKEN"))
        (println "trying to get token from env...")
        (System/getenv "MEOWKEN"))
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
