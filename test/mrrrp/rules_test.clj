(ns mrrrp.rules-test
  (:require [mrrrp.rules :refer :all]
            [malli.core :as m]
            [clojure.test :refer [testing is deftest]]))

(def rule1
  {:condition {:only-channels #{"some"}
               :only-users #{"a"}
               :text #"^text$"}
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
