(ns mrrrp.meowperhonsion-test
  (:require
   [clojure.test :refer [deftest is are]]
   [mrrrp.meowperhonsion :refer [extract-meows strip] :as c]))

(binding [c/append-catface str]
  (deftest stripper-test
    (are [in out] (= [in out]
                     [in (c/strip-trailing-catface in)])
      "mraa mrrrp" "mraa mrrrp"
      "meow :3" "meow"
      "mraow :3 " "mraow"
      "meow :3 meow" "meow :3 meow"
      "Gods, i hate trannies so much" "Gods, i hate trannies so much"))
  (deftest extract-meows-test-single
    (are [in out] (= [in out]
                     (cons in (extract-meows in)))
      "meow" "meow"
      "mraww" "mraww"
      "mraa mrrrp" "mraa mrrrp"
      "meow :3" "meow")))
