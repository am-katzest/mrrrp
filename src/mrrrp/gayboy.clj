(ns mrrrp.gayboy
  (:require [mrrrp.meowperhonsion :as re]))

(def gayboy-meow (re/re-str "^" re/meowgex "$"))
(str gayboy-meow)
(def gayboy-id  "204255221017214977")

(def channels-where-gayboy-meows (atom #{}))

(defn not-gayboy [uid] (not= gayboy-id uid))

(defn gayboy-in-channel? [cid] (@channels-where-gayboy-meows cid))

(defn is-gayboy-able-to-handle-this-message? [msg]
  (or (re/matches? gayboy-meow msg)
      (re/matches? #".*(:3|OwO|\^w\^|UwU).*" msg)))
(defn maybe-update-gayboy-meowing-area [uid cid msg]
  (when (and (= uid gayboy-id)
             ;; it means it meowed back at someone
             (is-gayboy-able-to-handle-this-message? msg))
    (swap! channels-where-gayboy-meows conj cid)
    (print "channels-where-gayboy-meows is now " @channels-where-gayboy-meows)))
