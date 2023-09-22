(ns mrrrp.finite-state-meowshine
  (:require [statecharts.core :as fsm]))
;; output of these functions decide what state to use
(defn only-channel [{:keys [cid]}] cid)

(defn user-in-channel [{:keys [cid uid]}] [cid uid])

(defmulti apply-event (fn [_ r] (:type r)))

(defmethod apply-event :fsm
  [event {:keys [machine location extract-type states] :as x}]
  (let [loc (location event)
        type (try  (extract-type event)
                   (catch Throwable t
                     {:extract-type-failed-with t}))
        event' (assoc event :type type)
        state (or (states loc) (fsm/initialize machine))
        state' (try (fsm/transition machine state event')
                    (catch Throwable t  ;inelegant, change
                      (prn "state transition failed" {:old-state state :event event' :exception t})
                      state))
        state'' (dissoc state' :fx)]
    {:fx (:fx state')
     :state (assoc-in x [:states loc] state'')}))

(defmethod apply-event :function
  [event {:keys [function] :as x}]
  {:fx (function event)
   :state x})

(defn run-event-through [fsms event]
  (let [together (mapv (partial apply-event event) (:state fsms))]
    {:state (mapv :state together)
     :fx (mapcat :fx together)}))

(def empty-fsms {:state []})
;; temporarliy contains :fx key for simplicity
(def execs (atom empty-fsms))

(defn make-fsm [machine location extract]
  {:type :fsm
   :machine machine
   :id (:id machine)
   :location location
   :extract-type extract
   :states {}})

(defn make-fn [f id]
  {:type :function
   :id id
   :function f})

(defn add-exec [x fsm] (update x :state conj fsm))

(defn reg-fsm! [& args]
  (let [fsm (apply make-fsm args)]
    (swap! execs add-exec fsm)))

(defn reg-fn! [& args]
  (let [fsm (apply make-fn args)]
    (swap! execs add-exec fsm)))

(defn addfx
  "adds side effects to fsm, used with :actions"
  [& ks]
  (fsm/assign (fn [s & _]
                (assoc s :fx ks))))

(defn reply [text] (addfx [:reply text]))

(defn accept-message! "returns fx" [event]
  (:fx (swap! execs run-event-through event)))

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
             (assoc s :uid (:uid e) :other-msgs 0))
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
                                   :actions (reply "i'm a commeownist!")}
                                  :something-different
                                  {:target :single-said
                                   :actions (fsm/assign increment)}}
                             :always [{:guard too-many-msgs
                                       :target :idle}]}}})))

(defn mentions-surgery? [{:keys [msg]}]
  (if (re-matches #"(?i).*(grs|srs|(top|bottom)\s+surgery|masectomy).*" msg)
    :mentions-surgery
    :something-different))
(reg-fsm! commeownist only-channel mentions-surgery?)

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

(defn uwu-owo? [{:keys [msg]}]
  (cond (re-matches #"(?i)uwu" msg) :uwu
        (re-matches #"(?i)owo" msg) :owo
        :else :neither))

(reg-fsm! uwu-owo user-in-channel uwu-owo?)

(as->
 (fsm/initialize commeownist) -
  (fsm/transition commeownist - {:uid :meow :type :mentions-surgery})
  (fsm/transition commeownist - {:uid :other :type :mentions-surgery}))
