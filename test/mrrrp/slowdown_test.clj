(ns mrrrp.slowdown-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.core.async :as a]
   [mrrrp.slowdown :as s]))


(defn output-times "pretends to be update-time, instead of giving current time, gives ones from list"
  [times]
  (let [queue (atom (cons nil times))]
    (fn [state]
      (assoc state :time (first (swap! queue rest))))))

(defn run-msg-through [strategy conf messages times]
  (let [input-ch (a/to-chan!! messages)
        output-ch (a/chan 100)
        timer (output-times times)]
    (println "inserting")
    (println "about to run")
    (s/run-slowdown timer strategy conf input-ch output-ch)
    (println "runed")
    (a/<!! (a/into [] output-ch))))

(def default-conf {:period 10 :count 2 :strategy s/dropping-strategy})

(deftest dropping-test
  (is (= ["meow" "meow"]
         (run-msg-through s/dropping-strategy default-conf
                          ["meow" "meow" "mraw" "mraw"]
                          [0 1 2 3])))
  (is (= ["meow" "meow" "mraow" "mraow"]
         (run-msg-through s/dropping-strategy default-conf
                          ["meow" "meow" "mraow" "mraow"]
                          [0 1 20 30])))
  (is (= ["meow" "meow" "mraow"]
         (run-msg-through s/dropping-strategy default-conf
                          ["meow" "meow" "mreu" "mraow"]
                          [0 5 6 11]))))

(deftest random-test
  (is (= ["meow" "meow"]
         (run-msg-through s/dropping-randomly-strategy
                          (assoc default-conf :count 2)
                          ["meow" "meow" "mraw" "mraw"]
                          [0 1 2 3])))
  (is (= ["meow" "meow" "mraw" "mraw"]
         (run-msg-through s/dropping-randomly-strategy
                          (assoc default-conf :count 10)
                          ["meow" "meow" "mraw" "mraw"]
                          [0 1 2 3])))
  (is (not=
       (run-msg-through s/dropping-randomly-strategy
                        {:period 20 :count 20}
                        (map str (range 20))
                        (range 20))
       (run-msg-through s/dropping-randomly-strategy
                        {:period 20 :count 20}
                        (map str (range 20))
                        (range 20)))))


(defn make-catcher "pretends to be respond, accumulates what it's called with"
  []
  (let [acc (atom [])]
    [acc (fn [_rest channel text]
           (swap! acc conj [channel text]))]))

(defn run-msg-through-router [conf messages times]
  (let [input-ch (a/to-chan!! messages)
        [responses catcher] (make-catcher)
        conf (merge conf
                    {:responder catcher
                     :update-time (output-times times)})]
    (s/run-router conf input-ch)
    (println "uwu" @responses)
    (Thread/sleep 10)
    @responses))

(deftest router-test
  (testing "two channels, two messages"
    (let [messages [{:channel "A" :text "meow"}
                    {:channel "B" :text "mraow"}]
          times [10 200]
          responses (run-msg-through-router default-conf messages times)]
      (is (= #{["A" "meow"] ["B" "mraow"]} (set responses)))))
  (testing "one channel saturated, other still receives"
    (let [messages [{:channel "A" :text "meow1"}
                    {:channel "A" :text "meow2"}
                    {:channel "B" :text "mraow"}]
          times [10 11 12]
          responses (run-msg-through-router default-conf messages times)]
      (is (= #{["A" "meow1"] ["A" "meow2"] ["B" "mraow"]} (set responses)))))
  (testing "blocking works"
    (let [messages [{:channel "A" :text "meow1"}
                    {:channel "A" :text "meow2"}
                    {:channel "A" :text "meow3"}]
          times [10 11 12]
          responses (run-msg-through-router default-conf messages times)]
      (is (= #{["A" "meow1"] ["A" "meow2"]} (set responses))))))
