(ns mrrrp.repliers
  (:require [mrrrp.meowperhonsion]
            [statecharts.core :as fsm]
            [mrrrp.finite-state-meowshine :refer :all]))

;┏┳┓┏━┓┏━╸╻ ╻╻┏┓╻┏━╸┏━┓
;┃┃┃┣━┫┃  ┣━┫┃┃┗┫┣╸ ┗━┓
;╹ ╹╹ ╹┗━╸╹ ╹╹╹ ╹┗━╸┗━┛
;; --- commeownist ---
;;  https://duckduckgo.com/?t=ftsa&q=top+surgery+bottom+surgery+i%27m+a+communist&iax=images&ia=images&iai=https%3A%2F%2Fpreview.redd.it%2Fqxgzc4h3f6h31.jpg%3Fwidth%3D640%26crop%3Dsmart%26auto%3Dwebp%26s%3Da71879f876110adcaf3c8b39b35c22b3d201bede
;; user1: something surgery
;; user2: something surgery (checking whether they mean top/bottom would make the interaction never happen)
;; bot: i'm a commeownist

(def commeownist
  (fsm/machine
   (letfn [(idle->single [s e]
             (assoc s :author (:author e) :other-msgs 0))
           (increment [s _]
             (update s :other-msgs inc))
           (too-many-msgs [s _]
             (< 5 (:other-msgs s)))
           (ids-different? [s e]
             (not= (:author s) (:author e)))]
     {:id :commeownist
      :initial :idle
      :states {:idle {:on {:mentions-surgery
                           {:target :single-said
                            :actions (fsm/assign idle->single)}
                           :something-different :idle}}
               :single-said {:on {:mentions-surgery
                                  {:target :idle
                                   :guard ids-different?
                                   :actions (reply "i'm a commeownist!")}
                                  :something-different
                                  {:target :single-said
                                   :actions (fsm/assign increment)}}
                             :always [{:guard too-many-msgs
                                       :target :idle}]}}})))

(defn mentions-surgery? [{:keys [content]}]
  (if (re-matches #"(?i).*(grs|srs|(top|bottom)\s+surgery|masectomy).*" content)
    :mentions-surgery
    :something-different))


;; --- uwu/owo ---
(def uwu-owo
  (fsm/machine
   {:id :uwu-owo
    :initial :neither
    :states {:neither {:on {:owo {:target :OwO
                                  :actions (reply "OwO")}
                            :uwu {:target :UwU
                                  :actions (reply "UwU")}
                            :neither :neither}}
             :UwU {:on {:owo {:target :OwO
                              :actions (reply "UwU")}
                        :uwu {:target :UwU
                              :actions (reply "UwU")}
                        :neither :neither}}
             :OwO {:on {:owo {:target :OwO
                              :actions (reply "OwO")}
                        :uwu {:target :UwU
                              :actions (reply "OwO")}
                        :neither :neither}}}}))

(defn uwu-owo? [{:keys [content]}]
  (cond (re-matches #"(?i)uwu" content) :uwu
        (re-matches #"(?i)owo" content) :owo
        :else :neither))

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

(def repliers
  [(make-fsm uwu-owo user-in-channel uwu-owo?)
   (make-fsm commeownist only-channel mentions-surgery?)
   (make-fn mrrrp.meowperhonsion/wrap-answer :meowperhension)
   (make-fn nuh-uh :nuh-uh)])
