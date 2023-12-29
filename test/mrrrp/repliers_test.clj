(ns mrrrp.repliers-test
  (:require [mrrrp.repliers :as sut]
            [clojure.test :refer [deftest testing is]]))

(deftest nuh-uh-test
  (testing "false"
    (is (= [] (sut/nuh-uh {:content ""})))
    (is (= [] (sut/nuh-uh {:content "meow"})))
    (is (= [] (sut/nuh-uh {:content "nuh uh :3"}))))
  (testing "true"
    (let [a1 (sut/nuh-uh {:content "nuh uh"})
          a2 (sut/nuh-uh {:content "nuh uh "})]
      (is (= a1 a2))
      (is (= :reply (ffirst a1)))
      (is (re-matches #"https.*png" (second (first a1))))))
  (testing "asia is getting smart"
    (let [a1 (sut/nuh-uh {:content "nuh  uh"})
          a2 (sut/nuh-uh {:content " nuh  uh "})]
      (is (= [[:reply "yuh  uh"]] a1 a2))))
  (testing "asia is getting smarter"
    (let [a1 (sut/nuh-uh {:content "nuh   uh"})
          a2 (sut/nuh-uh {:content " nuh   uh "})]
      (is (= [[:reply "yuh-uh >:3"]] a1 a2)))))
