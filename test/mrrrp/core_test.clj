(ns mrrrp.core-test
  (:require [mrrrp.core :refer :all]
            [clojure.core.async :as a]
            [com.stuartsierra.component :as component]
            [mrrrp.message-handler-test :as mht]
            [clojure.test :refer :all]))

(defrecord FakeConnection
           [events]
  component/Lifecycle
  (stop [component]
    (a/close! (:events component))
    component))

(defrecord FakeOutput
           [chan]
  component/Lifecycle
  (stop [component]
    (a/close! (:chan component))
    component))
(def config
  {:bot-id "self"
   :gayboy {:id #{"gayboy"}
              :meowback-chance 0.0}
   :rate-limit {:count 5
                :period 60}
   :secrets {:meowken "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}})

;; (defn mrrrp-system [input output]
;;   (component/system-map
;;    :connection (map->FakeConnection {:events input})
;;    :msgout (map->FakeOutput {:chan output})
;;    :responder (component/using
;;                (map->Responder {:config config})
;;                [:connection :msgout])))


(defn run-messages-through [config messages]
  (let [finished (promise)
        input (a/chan 100)
        output (a/chan 100)]
    (run-messsage-handler-loop
     {:config config
      :connection {:bot-id "self"
                   :events input}
      :msgout {:chan output}})
    (doseq [m messages]
      (a/>!! input [:message-create m]))
    (a/>!! input [:disconnect finished])
    @finished
    (a/close! output)
    (a/<!! (a/into [] output))))

(deftest run-messsage-handler-loop-test
  (testing "empty"
    (is (= []
           (->> []
                (mht/in-channel "channel")
                (run-messages-through config)))))
  (testing "irrelevant"
    (is (= []
           (->> [["someone" "something"]]
                (mht/in-channel "channel")
                (run-messages-through config)))))
  (testing "meow"
    (is (= [{:type :reply, :channel "channel", :text "meow!"}]
           (->> [["someone" "meow!"]]
                (mht/in-channel "channel")
                (run-messages-through config)))))
  (testing "state kept correctly"
    (is (= []
           (->> [["gayboy" "meow"]
                 ["someone" "meow"]]
                (mht/in-channel "channel")
                (run-messages-through config))))))

(deftest config-reading-test
  (is (= {:rate-limit {:count 5, :period 60},
          :gayboy {:id #{"gayboy"}
                   :meowback-chance 0.1},
          :secrets {:meowken "MEOWMEOWMEOWMEOWMEOWMEOWMEOWMEOWMEOWMEOWMEOWMEOWMEOW"}}
         (read-config "test/defs.edn" "test/config.edn"))))
