(ns mrrrp.message-handler-test
  (:require [mrrrp.message-handler :as s]
            [clojure.test :refer :all]))

(def state s/initial-state)
(def conf {:bot-id "self"})

(defn wrap-msg "prepares message map, inverse of the `unpack-message` interceptor"
  [channel author content]
  {:channel-id channel
   :content content
   :author {:id author}})

(defn in-channel [channel messages]
  (map (fn [[author content]] (wrap-msg channel author content))
       messages))

(defn user-in-channel [author channel messages]
  (map (fn [content] (wrap-msg channel author content))
       messages))

(defn run-messages-through [config messages]
  (loop [state s/initial-state
         [msg & rest] messages
         acc []]
    (if (nil? msg) {:state state :replies acc}
        (let [{state' :state output :fx}
              (s/handle-message config state msg)]
          (recur state' rest (conj acc output))))))

(defn output-texts [{:keys [replies]}]
  (map (fn [fxs] (map :text fxs)) replies))

(defn in->out [config msg]
  (->> msg
       (s/handle-message config s/initial-state )
       :fx
       (map :text)))

(def context1 {:event (wrap-msg "chan1" "user1" "meow!")
               :state s/initial-state
               :config conf})
(def context2 {:event (wrap-msg "chan1" "user1" "human stuff")
               :message {:author "user1"
                         :channel "chan1"
                         :content "meow!"}
               :state s/initial-state
               :config conf})

(deftest formatting-test
  (is (= {:author "user1"
          :channel "chan1"
          :content "meow!"} (:message ((:enter s/unpack-message) context1)))))

(deftest output-formatting-test
  (is (= [] (:fx ((:leave s/postprocess-replies) (assoc context2 :fx nil)))))
  (is (is (= [{:type :reply, :channel "chan1", :text "meow"}
              {:type :reply, :channel "chan1", :text "uwu"}]
             (:fx ((:leave s/postprocess-replies) (assoc context2 :fx [[:reply "meow"]
                                                                     [:reply "uwu"]])))))))

(deftest msg-handler-test
  (is (= [] (:fx (s/handle-message
              conf s/initial-state
              {:channel-id "channel"
               :content "--"
               :author {:id "user"}}))))
    (is (= [{:fx :reply
             :channel "channel"
             :text "meow!"}] (:fx (s/handle-message
                               conf s/initial-state
                               {:channel-id "channel"
                                :content "meow!"
                                :author {:id "user"}}))))
    (is (= [] (in->out conf (wrap-msg "channel" "user" "normal words"))))
    (is (= ["meow!"] (in->out conf (wrap-msg "channel" "user" "meow!"))))
    (testing "blacklist"
      (testing "full interaction"
        (is (= [["meow!"] [] [] [] ["meow!"]]
               (->> ["meow!" "stop meowing" "meow!" "start meowing" "meow!"]
                    (user-in-channel "user" "channel" )
                    (run-messages-through conf)
                    (output-texts)))))
      (testing "blacklisting"
        (is (= [["meow!"] [] []]
               (->> ["meow!" "stop meowing" "meow!"]
                    (user-in-channel "user" "channel" )
                    (run-messages-through conf)
                    (output-texts)))))
      (testing "unblacklisting when no blacklist"
        (is (= [["meow!"] [] ["meow!"]]
               (->> ["meow!" "start meowing" "meow!"]
                    (user-in-channel "user" "channel" )
                    (run-messages-through conf)
                    (output-texts)))))))
