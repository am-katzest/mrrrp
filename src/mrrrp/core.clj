(ns mrrrp.core
  (:gen-class)
  (:require
   [aero.core :as aero]
   [better-cond.core :as b]
   [clojure.core.async :as a :refer [chan close!]]
   [clojure.tools.logging :as log]
   [com.stuartsierra.component :as component]
   [discljord.connections :as discord-ws]
   [discljord.messaging :as discord-rest]
   [malli.core :as m]
   [malli.error :as me]
   [mrrrp.additional-repliers]
   [mrrrp.message-handler :refer [handle-message]]))

;; connection to discord
(defn init-connection! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn close-connection! [{:keys [rest events]}]
  (discord-rest/stop-connection! rest)
  (close! events))

(defrecord DCConnection
           [token events gateway rest bot-id]
  component/Lifecycle
  (start [component]
    (log/info "starting connection")
    (let [{:keys [rest gateway events] :as conn} (init-connection! (:token component) :guild-messages)
          started-correctly? (and rest gateway events)]
      (when (not started-correctly?)
        (close-connection! conn)
        (throw (Exception. "could not start the bot")))
      (let [c (->> @(discord-rest/get-current-user! rest)
                   :id
                   (assoc conn :bot-id)
                   (into component))]
        (log/info "started connection == " c)
        c)))
  (stop [component]
    (log/info "stopping connection")
    (close-connection! component)
    component))

;; relies message it receives via channel to discord
(defrecord msgOutput
           [config connection chan]
  component/Lifecycle
  (start [component]
    (let [chan (a/chan (a/sliding-buffer 3))
          rest (:rest connection)]
      (a/go-loop []
        (when-let [{:keys [channel text]} (a/<! chan)]
          (log/info "responding" (str text))
          @(discord-rest/create-message! rest channel :content (str text))
          (recur)))
      (assoc component :chan chan)))
  (stop [component]
    (a/close! (:chan component))
    component))

(defn run-bot!
  "pulls events off of the channel and calls
  handle-event with them, stops when it sees a :disconnect event."
  [{:keys [config connection msgout]}]
  (let [cfg (assoc config :bot-id (:bot-id connection))]
    (a/go-loop [state {}]
      (let [[event-type event-data] (a/<! (:events connection))]
        (case event-type
          :disconnect (deliver event-data nil)
          :message-create (let [[state' fxs] (handle-message cfg state event-data)]
                            (doseq [fx fxs]
                              (a/>! (:chan msgout) fx))
                            (recur state'))
          (recur state))))))

(defn stop-bot!
  "if event channel is still open, waits for the bot thread to exit"
  [channel]
  (let [p (promise)
        closed? (a/put! channel [:disconnect p])]
    (or closed? @p)))

(defrecord Responder
    ;; decides if/how to respond to each message
           [config connection msgout]
  component/Lifecycle
  (start [component]
    (log/info "starting responder")
    (run-bot! component)
    component)
  (stop [component]
    (log/info "stopping responder")
    (stop-bot! (:events (:connection component)))
    component))

(def conf-schema
  [:map
   [:rate-limit [:map
                 [:count pos-int?]
                 [:period pos-int?]]]
   [:admins [:vector :string]]
   [:secrets [:map
              [:meowken [:string {:min 50 :max 100}]]]]])

(defn read-config [conf-file]
  (b/cond
    :let [config (try (aero/read-config conf-file)
                      (catch java.io.IOException _
                        (throw (Exception. "couldn't read the config file"))))]

    (not (m/validate conf-schema config))
    (->> config (m/explain conf-schema) me/humanize (str "improper config:\n") Exception. throw)
    :else config))

(defn mrrrp-system [config]
  (let [token (-> config :secrets :meowken)
        conf (dissoc config :secrets)]
    (component/system-map
     :connection (map->DCConnection {:token token})
     :msgout (component/using
              (map->msgOutput {:config conf})
              [:connection])
     :responder (component/using
                 (map->Responder {:config conf})
                 [:connection :msgout]))))

(defonce running-system (atom nil))

(defn run-system! [args]
  (try
    (let [config (read-config args)
          system (mrrrp-system config)]
      (log/info "starting system...")
      (reset! running-system (.start system)))
    (catch
     Exception e
      (binding [*out* *err*]
        (log/error "failed because:" (ex-message e))))))

(defn -main [conf-file]
  (run-system! conf-file))

(comment
  (do (swap! running-system (fn [s] (when s (.stop s))))
      (run-system! "default_config.edn")))
