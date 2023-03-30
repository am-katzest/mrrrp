(ns mrrrp.meowperhonsion
  (:require [clojure.string :as s]
            [better-cond.core :as b]))

(def re-str (comp re-pattern str))

(defn re-pred [pred x] (some? (re-matches pred x)))

(defn any-order [a b]
  (re-str "(" a b "|" b a ")"))

(defn at-least-one [main single]
  (re-str "("
          main "+" single main "*"
          "|"
          main "*" single main "+"
          ")"))

;┏┳┓┏━╸┏━┓╻ ╻
;┃┃┃┣╸ ┃ ┃┃╻┃
;╹ ╹┗━╸┗━┛┗┻┛

(def meows (str "(m[- ])?"
                "("
                (s/join "|"
                        ["m+r*[yieao]*[ea]+[ieao]*[łwu]*[eao]*"
                         "m+r+[ieao]+[łwu]+"
                         "m+[oae]+r*[łwu]*"
                         "p+u*rr+"
                         "m[ynmpwreaouwi]+[włu]" ; this one should get most
                         (str "r+" (at-least-one "[nmwreaouwi]" "[rea]") "[iawłu]+")
                         "m+r+p+"
                         "m+r+a+"
                         "m+r+i+a+p+"
                         "m+y*a+o*w*"
                         "m+rr+"])
                ")"))

(def nyas #"(n[- ])?n+[mnyi]+a+")

(def obvious-meows #"nya|mrr+p?|mraow|meow")
(def obvious-catfaces #"uwu|owo|:3|^w^")

(def junk #"[^\p{L}0-9]")
(def single-meowgex (re-str "(?i)(" meows "|" nyas ")"))
(def single-meowgex-with-junk (re-str junk "*" single-meowgex junk "*"))
(def multi-meowgex (re-str single-meowgex "(" junk "+" single-meowgex ")*"))
(def meowgex (re-str "^ *" multi-meowgex "[?!~ ]*$"))

                                        ;╻ ╻   ╻ ╻
                                        ;┃ ┃┃ ┃┃ ┃
                                        ;┗━┛┗┻┛┗━┛

(def catfaces ["uwu" ":3" "^w^"])

(def just-catface #"(?i)(([^\p{Alpha}\s])[wvo_-]\2|>?:3c?|:.|uwu|owo)")
(def catface (re-str #"(\s+|^)" just-catface #"(\s+|$)"))
(def trailing-catface (re-str #"(?i) +" just-catface #"(\s*$)"))

(defn strip [x]
  (-> x
      (s/replace  catface " ")
      (s/replace #"^\s*" "")
      (s/replace #"\s*$" "")))

(defn strip-trailing-catface [x]
  (-> x
      (s/replace  trailing-catface " ")
      (s/replace #"^\s*" "")
      (s/replace #"\s*$" "")))

(defn just-cat-stuff? [x]
  (let [bare (-> x
                 (s/replace just-catface "")
                 (s/replace single-meowgex ""))]
    (re-pred #"(?i)^[^\p{L}0-9]*$" bare)))

(defn extract-meows [input]
  [(strip-trailing-catface input)])

(defn ^:dynamic append-catface
  ([meow]
   (str meow " " (rand-nth catfaces)))
  ([chance meow]
   (if (> chance (rand))
     (append-catface meow)
     meow)))

(defn dig-for-cat-stuff [x]
  (let [ans (->> x
                 (re-seq (re-str #"\B" just-catface #"\B" "|" obvious-catfaces "|" #"\b" single-meowgex #"\b" "|" obvious-meows))
                 (map first))]
    (when (pos? (count ans))
      ans)))

(defn answer [input]
  (->>
   (b/cond
     :let [s (strip-trailing-catface input)]
     ;; single meows
     (re-pred single-meowgex input) [(append-catface 0.2 input)] ; meow -> meow :3
     (re-pred single-meowgex-with-junk input) [input] ; meow! -> meow!
     (re-pred single-meowgex s) [(append-catface s)]  ; meow :3 -> meow ^w^
     (re-pred single-meowgex-with-junk s) [(append-catface s)] ; meow! :3 -> meow! ^w^
     ;; just meows
     (re-pred meowgex input) [(append-catface 0.4 s)]
     (re-pred meowgex s) [(append-catface s)]
     ;; meows mixed with catfaces
     (and (re-seq meowgex input) (just-cat-stuff? input)) [(append-catface s)]
     ;; just catface
     (re-pred just-catface input) [input]
     ;; dig meows
     :let [assorted-meows (dig-for-cat-stuff input)]
     (some? assorted-meows) (take 2 (shuffle assorted-meows)))
   (remove #{":s" "me"})))

(defn wrong-answer [x]
  (let [ans (answer x)]
    (when (and ans (seq ans) (< 0 (count ans)))
      (if (> 1/90 (rand))
        (do
          (Thread/sleep 2000)
          ["Gave up trying to execute custom command #:3 after 1 minute because there is already one or more instances of it being executed"])
        ans))))
(wrong-answer "meow")
