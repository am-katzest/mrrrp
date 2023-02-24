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
    "meow :3" "meow"
    "meow :3 meow" "meow meow"
    "niejestemmiałknięciem" "niejestemmiałknięciem"
    "mommy-milkers" "mommy-milkers"
    "Gods, i hate trannies so much" "Gods, i hate trannies so much"))
(deftest extract-meows-test-single
  (are [in out] (= [in out]
                   (cons in (extract-meows in)))
    "meow" "meow"
    "mraww" "mraww"
    "mraa mrrrp" "mraa mrrrp"
    "meow :3" "meow"))
