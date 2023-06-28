(ns mrrrp.gayboy-test
  (:require
   [clojure.test :refer [deftest is are]]
   [mrrrp.gayboy :as s]))

(deftest is-gayboy-able-to-handle-this-message?-test
  (are [truth msg] (= [truth msg] [(s/is-gayboy-able-to-handle-this-message? msg) msg])
    false "usnahulaurca"
    true "meow"
    true "mraaaw"
    true "kitty cat :3"
    true "aaa ^w^ aaa"))
