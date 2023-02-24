(ns mrrrp.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [mrrrp.meowperhonsion :as c]
            [better-cond.core :as b]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]]))

(def state (atom nil))

(def bot-id (atom nil))

(def config (edn/read-string (slurp "config.edn")))

(defmulti handle-event (fn [type _data] type))
(def gayboy-id  "204255221017214977")
(defmethod handle-event :message-create
  [_ {:keys [channel-id content author] :as _data}]
  (b/cond
    :when (not= @bot-id (:id author))
    :when (or (not= gayboy-id (:id author)) (rand-nth [true false false]))
    (= content "-stop-") (throw (ex-info "stop" {}))
    :when-let [answer (c/answer content)]
    :do (prn content "->" answer)
    :let [reply #(discord-rest/create-message! (:rest @state) channel-id :content  (str %))]
    (doseq [ans answer] (reply ans))))

(defmethod handle-event :default [_ _])

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

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guild-messages))
  (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
  (future (try
            (message-pump! (:events @state) handle-event)
            (finally (stop-bot! @state)))))
(comment
  (-main)
  (kill-bot! @state))
