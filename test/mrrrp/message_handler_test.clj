(ns mrrrp.message-handler-test
  (:require [mrrrp.message-handler :as s]
            [clojure.test :refer :all]))

(def state {})
(def conf {:bot-id "self"})

(defn wrap-msg "prepares message map" [channel author content]
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
        (let [[state' output]
              (s/handle-message state config msg)]
          (recur state' rest (conj acc output))))))

(defn output-texts [{:keys [replies]}]
  (map (fn [fxs] (map :text fxs)) replies))

(defn in->out [config msg]
  (->> msg
       (s/handle-message state config)
       second
       (map :text)))
(deftest msg-handler-test
  (is (= [state []] (s/handle-message
                     conf state
                     {:channel-id "channel"
                      :content "--"
                      :author {:id "user"}})))
  (is (= [state [{:type :reply
                  :channel "channel"
                  :text "meow!"}]] (s/handle-message
                                    conf state
                                    {:channel-id "channel"
                                     :content "meow!"
                                     :author {:id "user"}})))
  (is (= [] (in->out conf (wrap-msg "channel" "user" "normal words"))))
  (is (= ["meow!"] (in->out conf (wrap-msg "channel" "user" "meow!")))))
