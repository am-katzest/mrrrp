(ns mrrrp.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [mrrrp.meowperhonsion :as c]
            [better-cond.core :as b]
            [mrrrp.slowdown :as slow]
            [mrrrp.gayboy :as g]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]])
  (:gen-class))

(def state (atom nil))
(def bot-id (atom nil))

(def blacklist (atom #{}))

(defn- stop-meowing [id]
  (swap! blacklist conj id))
(defn- start-meowing [id]
  (swap! blacklist disj id))

(defmulti handle-event (fn [type _data] type))
(defmethod handle-event :default [_ _])
(defmethod handle-event :message-create
  [_ {:keys [channel-id content author]}]
  (b/cond
    (= content "stop meowing") (stop-meowing channel-id)
    (= content "start meowing") (start-meowing channel-id)
    :when (not (@blacklist channel-id))
    :when (not= @bot-id (:id author))
    :when (g/not-gayboy (:id author))
    :when-let [answer (c/wrong-answer content)]
    :do (prn content "->" answer)
    :let [reply #(discord-rest/create-message! (:rest @state) channel-id :content  (str %))]
    (doseq [ans answer] (slow/add channel-id #(reply ans)))))

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
