(ns mrrrp.message-handler-test
  (:require [mrrrp.message-handler :as s]
            [mrrrp.meowperhonsion :as meow]
            [clojure.test :refer :all]))

(def conf {:bot-id "self"
           :gayboy {:id #{"gayboy"}
                    :meowback-chance 0.0}})

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
       (s/handle-message config s/initial-state)
       :fx
       (map :text)))

(def context-raw {:event (wrap-msg "chan1" "user1" "meow!")
                  :state s/initial-state
                  :config conf})

(def context-formatted {:event (wrap-msg "chan1" "user1" "human stuff")
                        :message {:author "user1"
                                  :channel "chan1"
                                  :content "meow!"}
                        :state s/initial-state
                        :config conf})

(deftest formatting-test
  (is (= {:author "user1"
          :channel "chan1"
          :content "meow!"} (:message ((:enter s/unpack-message) context-raw)))))

(deftest output-formatting-test
  (is (= [] (:fx ((:leave s/postprocess-replies-interceptor) (assoc context-formatted :fx nil)))))
  (is (is (= [{:type :reply, :channel "chan1", :text "meow"}
              {:type :reply, :channel "chan1", :text "uwu"}]
             (:fx ((:leave s/postprocess-replies-interceptor) (assoc context-formatted :fx [[:reply "meow"]
                                                                                [:reply "uwu"]])))))))

(deftest blacklist-test
  (testing "unblacklisting"
    (is (= #{"other"}
           (->>  (-> context-formatted
                     (assoc-in [:state :blacklist] #{"other"})
                     (assoc-in  [:message :content] "start meowing"))
                 ((:enter s/update-blacklist-interceptor))
                 :state
                 :blacklist))))
  (testing "applying"
    (is (not
         (->>  (-> context-formatted
                   (assoc-in [:state :blacklist] #{"other"}))
               ((:enter s/apply-blacklist-interceptor))
               :stop)))
    (is (= true
           (->>  (-> context-formatted
                     (assoc-in [:state :blacklist] #{"chan1"}))
                 ((:enter s/apply-blacklist-interceptor))
                 :stop)))))

(deftest msg-handler-test
  (binding  [meow/append-catface (fn [& x] (last x))]       ; just making the thing we are testing more predictable instead of stubbing, i know it's bad
   (is (= [] (:fx (s/handle-message
                   conf s/initial-state
                   {:channel-id "channel"
                    :content "--"
                    :author {:id "user"}}))))
   (is (= [{:type :reply
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
                   (user-in-channel "user" "channel")
                   (run-messages-through conf)
                   (output-texts)))))
     (testing "blacklisting"
       (is (= [["meow!"] [] []]
              (->> ["meow!" "stop meowing" "meow!"]
                   (user-in-channel "user" "channel")
                   (run-messages-through conf)
                   (output-texts)))))
     (testing "unblacklisting when no blacklist"
       (is (= [["meow!"] [] ["meow!"]]
              (->> ["meow!" "start meowing" "meow!"]
                   (user-in-channel "user" "channel")
                   (run-messages-through conf)
                   (output-texts)))))
     (testing "ignoring-self"
       (is (= [["meow!"] []]
              (->> [["other" "meow!"] ["self" "meow!"]]
                   (in-channel "channel")
                   (run-messages-through conf)
                   (output-texts)))))
     (testing "ignoring-gayboy"
       (is (= [["meow"] [] []]
              (->> [["other" "meow"] ["gayboy" "meow"] ["other" "meow"]]
                   (in-channel "channel")
                   (run-messages-through conf)
                   (output-texts))))))))
