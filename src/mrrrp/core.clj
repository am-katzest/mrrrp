(ns mrrrp.core
  (:require
   [better-cond.core :as b]
   [clojure.core.async :refer [chan close!]]
   [malli.core :as m]
   [clojure.tools.logging :as log]
   [malli.error :as me]
   [aero.core :as aero]
   [discljord.connections :as discord-ws]
   [discljord.events :refer [message-pump!]]
   [discljord.messaging :as discord-rest]
   [mrrrp.additional-repliers]
   [mrrrp.effects :as fx]
   [mrrrp.finite-state-meowshine :as fsm]
   [mrrrp.gayboy :as g]
   [mrrrp.slowdown :as slow])
  (:gen-class))

(def connection (atom nil))

(def blacklist (atom #{}))

(defn- stop-meowing [id]
  (swap! blacklist conj id)
  (log/info "blacklist is now " @blacklist))

(defn- start-meowing [id]
  (swap! blacklist disj id)
  (log/info "blacklist is now " @blacklist))

(defn make-replier [channel-id]
  (let [send-msg #(discord-rest/create-message! (:rest @connection) channel-id :content  (str %))]
    (fn [msg] (slow/add channel-id #(send-msg  msg)))))

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
    :when (not= (:bot-id @connection) (:id author))
    :let [replier (make-useable-only-once (make-replier channel-id))]
    :do (g/maybe-update-gayboy-meowing-area (:id author) channel-id content)
    :when (or (g/not-gayboy (:id author)) (> 0.1 (rand)))
    :when  (not (and (g/gayboy-in-channel? channel-id)
                     (g/is-gayboy-able-to-handle-this-message? content)))
    :let [fx (fsm/accept-message! (prepare-event channel-id (:id author) content))]
    (not-empty fx) (fx/run-fxs fx replier)))

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn init-connection! [token]
  (let [s (start-bot! token :guild-messages)
        started-correctly? (every? some? ((juxt :rest :gateway :events) s))]
    (when (not started-correctly?) (throw (Exception. "could not start the bot")))
    (assoc s :bot-id (:id @(discord-rest/get-current-user! (:rest s))))))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

#_(defn run-bot!
    "Starts a process which pulls events off of the channel and calls
  handle-event with them, and stops when it sees a :disconnect event.
  This takes control of the current thread.

  The handle-event function takes the keyword event type, and the event
  data."
    [event-ch handle-event]
    (loop []
      (let [[event-type event-data] (a/<!! event-ch)]
        (try (handle-event event-type event-data)
             (catch Exception e
               (log/error e "Exception occurred in event handler.")))
        (when-not (= event-type :disconnect)
          (recur))))
    nil)
(defn run-bot! [connection]
  (try
    (message-pump! (:events connection) handle-event)
    (finally (stop-bot! connection))))

(def conf-schema
  [:map
   [:rate-limit [:map
                 [:count pos-int?]
                 [:period pos-int?]]]
   [:admins [:vector :string]]
   [:secrets [:map
              [:meowken [:string {:min 50 :max 100}]]]]])

(defn read-config [args]
  (b/cond
    (not= 1 (count args))
    (throw (Exception. "program takes the config file name as argument"))

    :let [config (try (aero/read-config (first args))
                      (catch java.io.IOException _
                        (throw (Exception. "couldn't read the config file"))))]

    (not (m/validate conf-schema config))
    (->> config (m/explain conf-schema) me/humanize (str "improper config:\n") Exception. throw)
    :else config))

(defn -main [& args]
  (try
    (let [conf (read-config args)]
      (reset! connection (-> conf :secrets :meowken init-connection!))
      (run-bot! @connection))
    (catch
     Exception e
      (binding [*out* *err*]
        (log/error "failed because:" (ex-message e))))
    (catch Throwable t
      (binding [*out* *err*]
        (log/error "something else happened :<" t)))))

(comment
  (-main "default_config.edn")
  (stop-bot! @connection))
