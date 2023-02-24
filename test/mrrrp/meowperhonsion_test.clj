(ns mrrrp.meowperhonsion-test
  (:require
   [clojure.test :refer [deftest is are]]
   [mrrrp.meowperhonsion :refer [extract-meows strip]]))

(deftest stripper-test
  (are [in out] (= [in out]
                   [in (strip in)])
    "meow" "meow"
    "mraww" "mraww"
    "mraa mrrrp" "mraa mrrrp"
    "meow :3" "meow"))
(deftest extract-meows-test-single
  (are [in out] (= [in out]
                   (cons in (extract-meows in)))
    "meow" "meow"
    "mraww" "mraww"
    "mraa mrrrp" "mraa mrrrp"
    "meow :3" "meow"))
