(ns mrrrp.slowdown
  (:require
   [clojure.tools.logging :as log]
   [better-cond.core :as b]))
;; discord auto-mutes bot when it sends more than 5 messages in 5 seconds,
;; and used api wrapper doesn't handle this  ;-;

(def ^:dynamic *max-messages* 3)
(def ^:dynamic *timeout* 15000)

(defn clean [lst time]
  (remove #(> (- time *timeout*) %) lst))

(defn time-to-next-expire [lst time]
  (b/cond
    (empty? lst) 0
    :let [earliest (apply min lst)
          earliest-expire (+ earliest *timeout*)
          wait-time (- earliest-expire time)]
    (pos? wait-time) wait-time
    :else 0))

(defn- execute-when-ready [lst f]
  (let [time (System/currentTimeMillis)
        lst (clean lst time)]
    (log/debugf "sent %d messages within time window" (count lst))
    (if (>= (count lst) *max-messages*)
      (do
        (log/debug "rate limiting myself")
        (Thread/sleep (min (time-to-next-expire lst time) *timeout*))
        (recur lst f))
      (do (f)
          (conj lst time)))))

(def waitlist "id -> agent" (atom {}))

(defn add "executes function as soon as possible" [id f]
  (when-not (@waitlist id)
    (swap! waitlist assoc id (agent '())))
  (let [a (@waitlist id)]
    (send a execute-when-ready f)))
