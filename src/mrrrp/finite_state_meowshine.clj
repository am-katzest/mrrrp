(ns mrrrp.finite-state-meowshin
  (:require [statecharts.core :as fsm]))

;; interaction 1: https://duckduckgo.com/?t=ftsa&q=top+surgery+bottom+surgery+i%27m+a+communist&iax=images&ia=images&iai=https%3A%2F%2Fpreview.redd.it%2Fqxgzc4h3f6h31.jpg%3Fwidth%3D640%26crop%3Dsmart%26auto%3Dwebp%26s%3Da71879f876110adcaf3c8b39b35c22b3d201bede
;; something surgery
;; something surgery (different user, checking whether they mean top/bottom is useless)
;; bot: i'm a commeownist

(def commeownist
  (fsm/machine
   (letfn [(idle->single [s e]
             (assoc s :uid (:uid e) :other-msgs 0))
           (single->idle [s e]
             ((:reply-fn e) "i'm a commeownist!")
             s)
           (increment [s _]
             (update s :other-msgs inc))
           (too-many-msgs [s _]
             (< 5 (:other-msgs s)))
           (ids-different? [s e]
             (not= (:uid s) (:uid e)))]
     {:id :commeownist
      :initial :idle
      :states {:idle {:on {:mentions-surgery
                           {:target :single-said
                            :actions (fsm/assign idle->single)}
                           :something-different :idle}}
               :single-said {:on {:mentions-surgery
                                  {:target :idle
                                   :guard ids-different?
                                   :actions (fsm/assign single->idle)}
                                  :something-different
                                  {:target :single-said
                                   :actions (fsm/assign increment)}}
                             :always [{:guard too-many-msgs
                                       :target :idle}]}}})))

(defn mentions-surgery? [{:keys [msg]}]
  (if (re-matches #"(?i).*(grs|srs|(top|bottom)\s+surgery|masectomy).*" msg)
    :mentions-surgery
    :something-different))

(defn only-channel [{:keys [cid]}] cid)

(defn user-in-channel [{:keys [cid uid]}] [cid uid])

(def starting-fsms
  (mapv (fn [x] (assoc x :states {}))
        [{:machine commeownist
          :location only-channel
          :extract-type mentions-surgery?}]))

;╻┏┓╻╺┳╸┏━╸┏━┓┏┓╻┏━┓╻  ┏━┓
;┃┃┗┫ ┃ ┣╸ ┣┳┛┃┗┫┣━┫┃  ┗━┓
;╹╹ ╹ ╹ ┗━╸╹┗╸╹ ╹╹ ╹┗━╸┗━┛

(defn apply-event [event {:keys [machine location extract-type states] :as x}]
  (let [loc (location event)
        type (extract-type event)
        event' (assoc event :type type)
        state (or (states loc) (fsm/initialize machine))
        state' (fsm/transition machine state event')]
    (assoc-in x [:states loc] state')))

(def fsms (atom starting-fsms))

(defn run-event-through [fsms event]
  (mapv (partial apply-event event) fsms))

(defn accept-message! [event]
  (swap! fsms run-event-through event))
