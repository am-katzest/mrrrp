(ns mrrrp.additional-repliers
  (:require [mrrrp.meowperhonsion]
            [mrrrp.finite-state-meowshine :as fsm]))
;; meowperhension
(fsm/reg-fn! mrrrp.meowperhonsion/wrap-answer :meowperhension)
;; nuh uh
(defn nuh-uh [e]
  (condp re-matches (:msg e)
    #"(?i)\s*nuh uh[\s!.]*"
    [[:reply "https://media.discordapp.net/attachments/1102713394420793384/1154765740084305960/yuh_uh2.png"]]
    ;; smartass cases
    #"(?i)\s*nuh  uh[\s!.]*"
    [[:reply "yuh  uh"]]
    #"(?i)\s*nuh   uh[\s!.]*"
    [[:reply "yuh-uh >:3"]]
    []))
(fsm/reg-fn! nuh-uh :nuh-uh)
