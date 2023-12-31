(ns mrrrp.slowdown-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.core.async :as a]
   [mrrrp.slowdown :as s]))


(defn output-times [times]
  (let [queue (atom (cons nil times))]
    (fn [state]
      (assoc state :time (first (swap! queue rest))))))

(defn run-msg-through [strategy conf messages times]
  (let [input-ch (a/chan 100)
        output-ch (a/chan 100)
        timer (output-times times)]
    (println "inserting")
    (a/onto-chan!! input-ch messages)
    (println "about to run")
    (s/run timer strategy conf input-ch output-ch)
    (println "runed")
    (a/<!! (a/into [] output-ch))))

(def default-conf {:period 10 :count 2})

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
