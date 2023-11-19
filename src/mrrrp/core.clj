(ns mrrrp.core
  (:require
   [better-cond.core :as b]
   [clojure.core.async :refer [chan close!] :as a]
   [malli.core :as m]
   [clojure.tools.logging :as log]
   [malli.error :as me]
   [aero.core :as aero]
   [discljord.connections :as discord-ws]
   [discljord.messaging :as discord-rest]
   [com.stuartsierra.component :as component]
   [mrrrp.additional-repliers]
   [mrrrp.effects :as fx]
   [mrrrp.finite-state-meowshine :as fsm]
   [mrrrp.gayboy :as g]
   [mrrrp.slowdown :as slow])
  (:gen-class))

(def blacklist (atom #{}))

(defn- stop-meowing [id]
  (swap! blacklist conj id)
  (log/info "blacklist is now " @blacklist))

(defn- start-meowing [id]
  (swap! blacklist disj id)
  (log/info "blacklist is now " @blacklist))

(defn make-replier [rest channel-id]
  (let [send-msg #(discord-rest/create-message! rest channel-id :content  (str %))]
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
      (let [{:keys [rest gateway events] :as conn} (init-connection! (:token component) :guild-messages)
            started-correctly? (and rest gateway events)]
        (when (not started-correctly?)
          (close-connection! conn)
          (throw (Exception. "could not start the bot")))
        (->> @(discord-rest/get-current-user! rest)
             :id
             (assoc conn :bot-id)
             (into component))))
    (stop [component]
      (close-connection! component)
      component))

;; replier

(defn handle-message
  [config connection {:keys [channel-id content author]}]
  (b/cond
    (= content "stop meowing") (stop-meowing channel-id)
    (= content "start meowing") (start-meowing channel-id)
    :when (not (@blacklist channel-id))
    :when (not= (:bot-id connection) (:id author))
    :let [replier (make-useable-only-once (make-replier (:rest connection) channel-id))]
    :do (g/maybe-update-gayboy-meowing-area (:id author) channel-id content)
    :when (or (g/not-gayboy (:id author)) (> 0.1 (rand)))
    :when  (not (and (g/gayboy-in-channel? channel-id)
                     (g/is-gayboy-able-to-handle-this-message? content)))
    :let [fx (fsm/accept-message! (prepare-event channel-id (:id author) content))]
    (not-empty fx) (fx/run-fxs fx replier)))

(defn run-bot!
  "pulls events off of the channel and calls
  handle-event with them, stops when it sees a :disconnect event."
  [connection config]
  (loop []
    (let [[event-type event-data] (a/<!! (:events connection))]
      (case event-type
        :disconnect (deliver event-data nil)
        :message-create (do (handle-message config connection event-data)
                            (recur))
        (recur)))))

(defn stop-bot!
  "if event channel is still open, waits for the bot thread to exit"
  [channel]
  (let [p (promise)
        closed? (a/put! channel [:disconnect p])]
    (or closed? @p)
    ))

(defrecord Responder
    ;; decides if/how to respond to each message
    [config connection]
    component/Lifecycle
    (start [component]
      (future (run-bot! (:channel (:connection component)) config))
      component)
    (stop [component]
      (stop-bot! (:connection component))))

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


(defn mrrrp-system [config]
  (let [token (-> config :secrets :meowken)
        conf (dissoc config :secrets)]
    (component/system-map
     :connection (map->DCConnection {:token token})
     :responder (component/using
                 (map->Responder {:config conf})
                 [:connection]))))

(defn -main [& args]
  (try
    (let [config (read-config args)
          system (mrrrp-system config)]
      (.start system))
    (catch
        Exception e
        (binding [*out* *err*]
          (log/error "failed because:" (ex-message e))))
    (catch Throwable t
      (binding [*out* *err*]
        (log/error "something else happened :<" t)))))

(comment
  (-main "default_config.edn"))
