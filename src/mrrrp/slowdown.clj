(ns mrrrp.slowdown
  (:require
   [clojure.tools.logging :as log]
   [clojure.core.async :as a]
   [discljord.messaging :as discord-rest]
   [better-cond.core :as b]))
;; discord auto-mutes bot when it sends more than 5 messages in 5 seconds,
;; and used api wrapper doesn't handle this  ;-;

;; solution

;; incoming messages are split into channels for welp. channels,
;; each of them is handled by an (created as needed) instance of these things

(defn clean "discard times too old to matter from :previous" [{:keys [previous conf time] :as state}]
  (let [cleaned (remove #(> (- time (:period conf)) %) previous)]
    (log/debug previous "->" cleaned)
    (assoc state :previous cleaned)))

(defn accept-time "move current time to :previous" [{:keys [time] :as state}]
  (update state :previous conj time))

(defn update-time [state]
  (assoc state :time (System/currentTimeMillis)))

(defn time-to-next-expire [{:keys [previous conf time]}]
  (b/cond
    (empty? previous) 0
    :let [earliest (apply min previous)
          earliest-expire (+ earliest (:period conf))
          wait-time (- earliest-expire time)]
    (pos? wait-time) wait-time
    :else 0))

(defn run-slowdown [conf input output]
  (let [state {:conf conf
               :previous []}
        update-time (:update-time conf)
        strategy (:strategy conf)]
    (a/go-loop [state state]
      (if-let [val (a/<! input)]
        (let [state' (-> state update-time clean)
              [decision time] (strategy state')]
          (log/debug "decision on `" val "` is " decision)
          (case decision
            :drop (recur state')
            :pass (do
                    (a/>! output val)
                    (recur (accept-time state')))
            :wait (do (a/<! (a/timeout time)) ; sleep
                      (a/>! output val)
                      (recur (-> state' update-time accept-time)))))
        (do
          (log/debug "shutting down slowdown")
          (a/close! output))))))

(defn dropping-strategy
  [{:keys [conf previous]}]
  [(if (>= (count previous) (:count conf))
     :drop
     :pass)])

(defn dropping-randomly-strategy
  [{:keys [conf previous]}]
  (let [current (count previous)
        max (:count conf)
        ratio (/ current max)
        probability (- (* 2  ratio) 1)]
    ;; probability to drop is still 0% when halfway to timeout, reaches 100% when at the timeout
    [(if (> (rand) probability) :pass :drop)]))

; this part manages all these different paths
; this is not reinventing `pub`, it's lazy and only requires subset of it's functionality

(defn respond [rest channel text]
  (log/info "responding `" (str text) "` to " channel)
  @#_{:clj-kondo/ignore [:invalid-arity]}
   (discord-rest/create-message! rest channel :content text))

(defn run-responder [conf chan finished]
  (a/go-loop []
    (if-let [{:keys [channel text]} (a/<! chan)]
      (do ((:responder conf) (:rest conf) channel text)
          (recur))
      (deliver finished true))))

(defn new-channel [conf]
  (let [relay (a/chan 1)
        input (a/chan 3)
        finished (promise)]
    (run-responder conf relay finished)
    ;; for now we just always use dropping-randomly-strategy
    ;; it's separated for ease of testing and possibly to
    ;; allow different strategies for different channels
    (run-slowdown conf input relay)
    {:input input :finished finished}))

(defn ensure-route-has-channel [conf channels msg]
  (if (contains? channels (:channel msg))
    channels
    (assoc channels (:channel msg) (new-channel conf))))

(defn- close-all-channels [channels]
  (doseq [chan (vals channels)]
    (a/close! (:input chan)))
  (doseq [chan (vals channels)]
    @(:finished chan)))

(defn run-router [conf input-ch finished]
  (a/go-loop [channels {}]
    (if-let [msg (a/<! input-ch)]
      (let [channels' (ensure-route-has-channel conf channels msg)]
        (a/>! (get-in channels' [(:channel msg) :input]) msg)
        (recur channels'))
      (do (close-all-channels channels)
          (deliver finished true)))))

(def default-conf
  {:responder respond
   :update-time update-time
   :strategy dropping-randomly-strategy})
