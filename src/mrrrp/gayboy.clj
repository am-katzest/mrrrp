(ns mrrrp.gayboy
  (:require [mrrrp.meowperhonsion :as re]))

(def gayboy-meow (re/re-str "^" re/meowgex-with-junk "$"))
(str gayboy-meow)
(def gayboy-id  "204255221017214977")

(def channels-where-gayboy-meows (atom #{}))

(defn not-gayboy [uid] (not= gayboy-id uid))

(defn gayboy-in-channel? [cid] (@channels-where-gayboy-meows cid))

(defn gayboy-message? [msg] (re/matches? gayboy-meow msg))

(defn maybe-update-gayboy-meowing-area [uid cid msg]
  (when (and (= uid gayboy-id)
             (gayboy-message? msg))
    (swap! channels-where-gayboy-meows conj cid)))
