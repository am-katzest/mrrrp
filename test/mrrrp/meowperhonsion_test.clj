(ns mrrrp.meowperhonsion-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [mrrrp.meowperhonsion :refer [extract-meows strip answer] :as c]))
(defmacro meowbacks [& pairs]
  `(are [in out] (= [in out]
                    [in (answer in)]) ~@pairs))
(deftest meowperhension-test-no-appending
  (binding [c/append-catface (fn [& x] (last x))]
    (testing "stripper-test"
      (are [in out] (= [in out]
                       [in (c/strip-trailing-catface in)])
        "mraa mrrrp" "mraa mrrrp"
        "meow :3" "meow"
        "mraow :3 " "mraow"
        "meow :3 meow" "meow :3 meow"
        "Gods, i meow so much" "Gods, i meow so much"))
    (testing "extract-meows-test-single"
      (are [in out] (= [in out]
                       (cons in (extract-meows in)))
        "meow" "meow"
        "mraww" "mraww"
        "mraa mrrrp" "mraa mrrrp"
        "meow :3" "meow"))
    (testing "it-knows-what-catfaces-arent"
      (meowbacks
       ":3" [":3"]
       "" []
       "randomword:3" [":3"]
       "unwanted:pattention" []
       "<:patpat:1089585540388626502>" []
       ":p" [":p"]))
    (testing "it-behaves"
      (meowbacks
       "coś ma" []
       "hey Ma!" []
       "touch me" []
       "kitty: cat" []
       "poor" []))
    (testing "it-isn't-caninephobic"
      (meowbacks
       "awooooomnia" ["awooooo"]
       "woof!" ["woof!"]))
    (testing "stops-awo-ing"
      (meowbacks
       "wordawo" []
       "w prawo skręcaj" []))
    (testing "polskie szczekania"
      (meowbacks
       "hał hał" ["hał hał"]
       "hauu" ["hauu"]
       "szczek" ["szczek"]))
    (testing "brak ramion"
      (meowbacks
       "wznieś ramiona" []))))
