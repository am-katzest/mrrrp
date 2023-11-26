(ns mrrrp.gayboy-test
  (:require
   [clojure.test :refer :all]
   [mrrrp.gayboy :as s]
   [mrrrp.message-handler-test :as t]))

(deftest is-gayboy-able-to-handle-this-message?-test
  (are [truth msg] (= [truth msg] [(s/is-gayboy-able-to-handle-this-message? msg) msg])
    false "usnahulaurca"
    true "meow"
    true "mraaaw"
    true "kitty cat :3"
    true "aaa ^w^ aaa"))

(deftest interceptor-test
  (testing "recognizing"
    (is (= #{"chan1"}
           (->>  (-> t/context-formatted
                     (update :message merge {:author "gayboy" :content "meow"}))
                 ((:enter s/update-gayboy-interceptor))
                 :state
                 :gayboy-channels)))
    (is (= #{}
           (->>  (-> t/context-formatted
                     (update :message merge {:author "gayboy" :content "something else"}))
                 ((:enter s/update-gayboy-interceptor))
                 :state
                 :gayboy-channels)))
    (is (= #{}
           (->>  (-> t/context-formatted
                     (update :message merge {:author "other" :content "meow"}))
                 ((:enter s/update-gayboy-interceptor))
                 :state
                 :gayboy-channels))))
  (testing "applying"
    (testing "ignoring others"
      (is (= true
             (->>  (-> t/context-formatted
                       (update :state merge {:gayboy-channels #{"chan1"}})
                       (update :message merge {:author "other" :content "meow"}))
                   ((:enter s/ignore-gayboy-interceptor))
                   :stop
                   ))))
    (testing "not ignoring others"
      (is (= nil
             (->>  (-> t/context-formatted
                       (update :state merge {:gayboy-channels #{"other-channel"}})
                       (update :message merge {:author "other" :content "meow"}))
                   ((:enter s/ignore-gayboy-interceptor))
                   :stop
                   ))))
    (testing "ignoring gayboy" (is (= true
            (->>  (-> t/context-formatted
                      (update :state merge {:gayboy-channels #{"chan1"}})
                      (update :message merge {:author "gayboy" :content "meow"}))
                  ((:enter s/ignore-gayboy-interceptor))
                  :stop
                  ))))))
