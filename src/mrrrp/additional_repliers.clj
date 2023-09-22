(ns mrrrp.additional-repliers
  (:require [mrrrp.meowperhonsion]
            [mrrrp.finite-state-meowshine :as fsm]))
;; meowperhension
(fsm/reg-fn! mrrrp.meowperhonsion/wrap-answer :meowperhension)
;; nuh uh
(defn nuh-uh [e]
  (if (re-matches #"(?i)\s*nuh uh[\s!.]*" (:msg e))
    [[:reply "https://media.discordapp.net/attachments/1102713394420793384/1154765740084305960/yuh_uh2.png"]]
    []))
(fsm/reg-fn! nuh-uh :nuh-uh)
