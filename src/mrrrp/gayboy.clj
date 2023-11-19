(ns mrrrp.gayboy
  (:require
   [clojure.tools.logging :as log]
   [mrrrp.meowperhonsion :as re]))

(def gayboy-meow (re/re-str "^" re/meowgex "$"))
(str gayboy-meow)
(def gayboy-id  "204255221017214977")   ;TODO move to config

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
    (log/info "channels-where-gayboy-meows is now " @channels-where-gayboy-meows)))
