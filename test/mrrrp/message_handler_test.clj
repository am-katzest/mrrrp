(ns mrrrp.message-handler-test
  (:require [mrrrp.message-handler :as t]
            [clojure.test :refer :all]))

(def state {})
(def conf {:bot-id "self"})

(deftest msg-handler-test
  (is (= [state []] (t/handle-message
                     conf state
                     {:channel-id "test"
                      :content "--"
                      :author {:id "other"}})))
  (is (= [state [{:type :reply
                  :channel "test"
                  :text "meow!"}]] (t/handle-message
                                    conf state
                                    {:channel-id "test"
                                     :content "meow!"
                                     :author {:id "other"}}))))
