(ns mrrrp.meowperhonsion-test
  (:require
   [clojure.test :refer [deftest is are]]
   [mrrrp.meowperhonsion :refer [extract-meows strip answer] :as c]))

(binding [c/append-catface str]
  (deftest stripper-test
    (are [in out] (= [in out]
                     [in (c/strip-trailing-catface in)])
      "mraa mrrrp" "mraa mrrrp"
      "meow :3" "meow"
      "mraow :3 " "mraow"
      "meow :3 meow" "meow :3 meow"
      "Gods, i meow so much" "Gods, i meow so much"))
  (deftest extract-meows-test-single
    (are [in out] (= [in out]
                     (cons in (extract-meows in)))
      "meow" "meow"
      "mraww" "mraww"
      "mraa mrrrp" "mraa mrrrp"
      "meow :3" "meow"))
  (deftest it-knows-what-catfaces-arent
    (are [in out] (= [in out]
                     [in (answer in)])
      ":3" [":3"]
      "" []
      "randomword:3" [":3"]
      "unwanted:pattention" []
      "<:patpat:1089585540388626502>" []
      ":p" [":p"]))
  (deftest it-behaves
    (are [in out] (= [in out]
                     [in (answer in)])
      "coś ma" []
      "hey Ma!" []
      "touch me" []
      "kitty: cat" []
      "poor" []))
  (deftest it-isn't-caninephobic
    (are [in out] (= [in out]
                     [in (answer in)])
      "awooooomnia" ["awooooo"]
      "woof!" ["woof!"])))
