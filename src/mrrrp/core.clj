(ns mrrrp.core
  (:require
   [better-cond.core :as b]
   [clojure.core.async :refer [chan close!]]
   [clojure.string :as str]
   [discljord.connections :as discord-ws]
   [discljord.events :refer [message-pump!]]
   [discljord.messaging :as discord-rest]
   [mrrrp.additional-repliers]
   [mrrrp.effects :as fx]
   [mrrrp.finite-state-meowshine :as fsm]
   [mrrrp.gayboy :as g]
   [mrrrp.slowdown :as slow])
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

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

(defn  kill-bot! [{:keys [rest gateway events] :as _state}]
  (discord-ws/disconnect-bot! gateway))

(defmacro exp->some [& exprs]
  `(try ~@exprs
        (catch Throwable ~(gensym) nil)))

(defn get-token []
  (some-> (b/cond
            :do (println "trying to get token from env...")
            :let [env-content (System/getenv "MEOWKEN")]
            (some? env-content) env-content

            :do (println "trying to get token from file...")
            :let [file-content (exp->some (slurp "MEOWKEN"))]
            (some? file-content) file-content)
          str/trim))

(defn -main [& _]
  (if-let [token (exp->some (get-token))]
    (do
      (let [s (start-bot! token :guild-messages)]
        (println s)
        (when-not (every? some? ((juxt :rest :gateway :events) s))
          (println "could not start the bot")
          (System/exit 1))
        (reset! state s))
      (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
      (future (try
                (message-pump! (:events @state) handle-event)
                (finally (stop-bot! @state)
                         (System/exit 0)))))
    (do (println "could not acquire token")
        (System/exit 1))))

(comment
  (-main)
  (kill-bot! @state))
