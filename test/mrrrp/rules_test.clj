(ns mrrrp.rules-test
  (:require [mrrrp.rules :refer :all]
            [malli.core :as m]
            [clojure.test :refer [testing is deftest]]))

(def rule1
  {:condition {:only-channels #{"chan" "other"}
               :only-authors #{"author"}
               :text #"^meow$"}
   :action [:reply "reply"]})

(def rule2
  {:condition {:only-servers #{"srv"}
               :text #"^text$"}
   :action [:reply "reply"]})

(deftest rule-validation
  (testing "condition validation"
    (is (m/validate condition-schema (:condition rule1)))
    (is (m/validate condition-schema (:condition rule2)))
    (is (m/validate action-schema (:action rule2))))
  (is (m/validate rule-schema rule1)))

(def context1 {:event {:guild-id "guild"}
               :message {:author "author"
                         :content "meow"
                         :channel "chan"}})

(deftest rule-predicate-test
  (testing "text"
    (is (applies? context1 {:condition {:text #".*"}}))
    (is (not (applies? context1 {:condition {:text #"wrong"}}))))
  (testing "server"
    (is (applies? context1 {:condition {:text #".*" :only-servers #{"guild"}}}))
    (is (not (applies? context1 {:condition {:text #".*" :only-servers #{"wrong"}}}))))
  (testing "channel"
    (is (applies? context1 {:condition {:text #".*" :only-channels #{"chan"}}}))
    (is (not (applies? context1 {:condition {:text #".*" :only-channels #{"wrong"}}}))))
  (testing "author"
    (is (applies? context1 {:condition {:text #".*" :only-authors #{"author"}}}))
    (is (not (applies? context1 {:condition {:text #".*" :only-authors #{"wrong"}}}))))
  (testing  "mixed"
    (is (applies? context1 {:condition {:text #".*" :only-authors #{"author" "other" "blah"} :only-channels #{"chan"}}}))
    (is (not (applies? context1 {:condition {:text #".*" :only-servers #{"wrong"} :only-channels #{"chan"}}})))))

(deftest rule-applying-test
  (is (= nil (actions context1 rule2)))
  (is (= [[:reply "reply"]] (actions context1 rule1))))

(deftest context-manipulation-test
  (is (= [[:reply "reply"]]
         (->> (assoc context1 :config {:rules [rule1]})
              ((:enter apply-rules-interceptor))
              :fx)))
  (is (= [[:reply "reply"]]
         (->> (assoc context1 :config {:rules [rule1 rule2]})
              ((:enter apply-rules-interceptor))
              :fx)))
  (is (= [[:reply "reply"] [:reply "reply"]]
         (->> (assoc context1 :config {:rules [rule1 rule1]})
              ((:enter apply-rules-interceptor))
              :fx)))
  (is (= nil
         (->> (assoc context1 :config {:rules [rule2 rule2]})
              ((:enter apply-rules-interceptor))
              :fx))))
