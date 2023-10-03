(ns mrrrp.additional-repliers-test
  (:require [mrrrp.additional-repliers :as sut]
            [clojure.test :refer [deftest testing is]]))

(deftest nuh-uh-test
  (testing "false"
    (is (= [] (sut/nuh-uh {:msg ""})))
    (is (= [] (sut/nuh-uh {:msg "meow"})))
    (is (= [] (sut/nuh-uh {:msg "nuh uh :3"}))))
  (testing "true"
    (let [a1 (sut/nuh-uh {:msg "nuh uh"})
          a2 (sut/nuh-uh {:msg "nuh uh "})]
      (is (= a1 a2))
      (is (= :reply (ffirst a1)))
      (is (re-matches #"https.*png" (second (first a1))))))
  (testing "asia is getting smart"
    (let [a1 (sut/nuh-uh {:msg "nuh  uh"})
          a2 (sut/nuh-uh {:msg " nuh  uh "})]
      (is (= [[:reply "yuh  uh"]] a1 a2)))))
