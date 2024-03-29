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
              :content))

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
  (let [e1 {:author 0 :channel 0 :content :don't}
        e2  {:author 0 :channel 0 :content :meow}
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
  (let [e1 {:author 0 :channel 0 :content :don't}
        e2  {:author 0 :channel 0 :content :meow}
        group [meow meow]]
    (testing "creation"
      (is (= [:meow :meow]
             (->> group (map :id)))))
    (testing "gathering-fx"
      (is (empty? (:fx (s/run-event-through group e1))))
      (is (= [[:reply "meow"] [:reply "meow"]]
             (:fx (s/run-event-through group e2)))))))

(defn mrow [{:keys [content]}] (if (= content :meow) [[:reply "mrow"]] []))

(deftest group-of-fns
  (let [e1 {:author 0 :channel 0 :content :don't}
        e2  {:author 0 :channel 0 :content :meow}
        group [(s/make-fn mrow :mrow)
               meow]]
    (testing "creation"
      (is (= [:mrow :meow]
             (->> group (map :id)))))
    (testing "gathering-fx"
      (is (empty? (:fx (s/run-event-through group e1))))
      (is (= [[:reply "mrow"] [:reply "meow"]]
             (:fx (s/run-event-through group e2)))))))

(deftest wrapping-functions
  (let [f (constantly nil)
        wrapped (s/make-fn f :nil)]
    (is (= :function (:type wrapped)))
    (is (= f (:function wrapped)))))
