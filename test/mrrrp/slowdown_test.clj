(ns mrrrp.slowdown-test
  (:require
   [clojure.test :refer [deftest is]]
   [mrrrp.slowdown :as s]))

(deftest some-tests
  (binding [s/timeout 10]
    (is (= [7 8]
           (s/clean [2 7 8 3] 15)))
    (is (= 0 (s/time-to-next-expire [] 20)))
    (is (= 8 (s/time-to-next-expire [8 9 15] 10)))
    (is (= 3 (s/time-to-next-expire [8 9 15] 15)))
    (is (= 0  (s/time-to-next-expire [8 9 15] 20)))))
