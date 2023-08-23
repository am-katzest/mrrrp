(ns mrrrp.finite-state-meowshine-test
  (:require [mrrrp.finite-state-meowshine :as s]
            [statecharts.core :as fsm]
            [clojure.test :refer :all]))

(def meow-machine (fsm/machine
                   {:id :meow
                    :initial :only
                    :states
                    {:only
                     {:on
                      {:meow {:actions (s/reply "meow")}
                       :don't {}}}}}))
(def flip-flop
  (s/make-fsm (fsm/machine
               {:id :ff
                :initial :A
                :states
                {:A {:on {:event {:target :B}}}
                 :B {:on {:event {:target :A}}}}})
              (constantly nil)
              (constantly :event)))
(def meow
  (s/make-fsm meow-machine
              (constantly nil)
              :msg))
(deftest inserting-fx
  (is (= [[:reply "meow"]]
         (as->
          (fsm/initialize meow-machine) -
           (fsm/transition meow-machine - {:type :meow})
           (:fx -))))
  (is (empty?
       (as->
        (fsm/initialize meow-machine) -
         (fsm/transition meow-machine - {:type :don't})
         (:fx -)))))

(deftest event-apply-fsm
  (let [e1 {:uid 0 :cid 0 :msg :don't}
        e2  {:uid 0 :cid 0 :msg :meow}
        meow-e1 (s/apply-event e1 meow)
        meow-e2 (s/apply-event e2 meow)]
    (testing "state was initialised"
      (is (= :only (-> meow-e1 :state :states (get nil) :_state))))
    (testing "replies"
      (is (= nil (-> meow-e1 :fx)))
      (is (= [[:reply "meow"]] (-> meow-e2 :fx))))
    (testing "keeps state"
      (let [e {}
            s0 flip-flop
            {s1 :state} (s/apply-event e s0)
            {s2 :state}  (s/apply-event e s1)]
        (is (= :B (-> s1  :states (get nil) :_state)))
        (is (= :A (-> s2  :states (get nil) :_state)))))))

(deftest group-of-fsms
  (let [e1 {:uid 0 :cid 0 :msg :don't}
        e2  {:uid 0 :cid 0 :msg :meow}
        group (-> s/empty-fsms
                  (s/add-fsm meow)
                  (s/add-fsm meow))]
    (testing "creation"
      (is (= [:meow :meow]
             (->> group :state (map (comp :id :machine))))))
    (testing "gathering-fx"
      (is (empty? (:fx (s/run-event-through group e1))))
      (is (= [[:reply "meow"] [:reply "meow"]]
             (:fx (s/run-event-through group e2)))))))
