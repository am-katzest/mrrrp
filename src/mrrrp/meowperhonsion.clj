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
(def  woofss (str #"([wa][- ])" (s/join "|"
                                        ["w+o+[ao]+f*"
                                         "a+w+(u+o+)f*"
                                         "[ar]*w+r*(u+|o+)f+"
                                         "a+r+f+"
                                         "r+(o+|u+)f+"
                                         "p+(a+|o+)r*"
                                         "p+(o+|a+)r*"
                                         "b+a+r+kf*"])))

(def obvious-meows #"nya|mrr+p?|mraow|meow")
(def obvious-catfaces #"uwu|owo|:3|^w^|B3")

(def junk #"[^\p{L}0-9]")
(def single-woofgex (re-str "(?i)(" woofss ")"))
(def single-meowgex (re-str "(?i)(" meows "|" nyas ")"))
(def single-meowgex-with-junk (re-str junk "*" single-meowgex junk "*"))
(def single-woofgex-with-junk (re-str junk "*" single-woofgex junk "*"))
(def multi-meowgex (re-str single-meowgex "(" junk "+" single-meowgex ")*"))
(def multi-woofgex (re-str single-woofgex "(" junk "+" single-woofgex ")*"))
(def meowgex (re-str "^ *" multi-meowgex "[?!~ ]*$"))
(def  woofgex (re-str "^ *" multi-woofgex "[?!~ ]*$"))

                                        ;╻ ╻   ╻ ╻
                                        ;┃ ┃┃ ┃┃ ┃
                                        ;┗━┛┗┻┛┗━┛

(def catfaces [">:3" ":p" "OwO"])

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
                 (re-seq (re-str #"\b" just-catface #"\b" "|" obvious-catfaces "|" #"\b" single-meowgex #"\b" "|" obvious-meows))
                 (map first))]
    (when (pos? (count ans))
      ans)))

(defn woofs
  ([]
   (rand-nth ["woof" "Woof" "bark" "bark!" "BARK" "arf" "arf!" "yep" "ruff" "ruff!" "roff" "rwoff" "freh" "woof" "bark" "awooooo"]))
  ([n]
   (s/join " " (repeatedly n woofs))))

(defn answer [input]
  (->>
   (b/cond
     :let [s (strip-trailing-catface input)]
     ;; single meows
     (re-pred single-woofgex input) [(append-catface 0.2 input)] ; meow -> meow :3
     (re-pred single-woofgex-with-junk input) [input] ; meow! -> meow!
     (re-pred single-woofgex s) [(append-catface s)]  ; meow :3 -> meow ^w^
     (re-pred single-woofgex-with-junk s) [(append-catface s)] ; meow! :3 -> meow! ^w^
     (re-pred single-meowgex input) [(append-catface 0.2 (woofs))] ; meow -> meow :3
     (re-pred single-meowgex-with-junk input) [(woofs)] ; meow! -> meow!
     (re-pred single-meowgex s) [(append-catface (woofs))]  ; meow :3 -> meow ^w^
     (re-pred single-meowgex-with-junk s) [(woofs)] ; meow! :3 -> meow! ^w^
     ;; just meows
     (re-pred woofgex input) [(append-catface 0.4 s)]
     (re-pred woofgex s) [(append-catface s)]
     (re-pred meowgex input) [(woofs 2)]
     (re-pred meowgex s) [(append-catface (woofs 3))]
     ;; meows mixed with catfaces
     (and (re-seq woofgex input) (just-cat-stuff? input)) [(append-catface s)]
     (and (re-seq meowgex input) (just-cat-stuff? input)) [(append-catface (woofs 5))]
     ;; just catface
     (re-pred just-catface input) [input]
     ;; dig meows
     :let [assorted-meows (dig-for-cat-stuff input)]
     (some? assorted-meows) [(woofs)])
   (remove #(#{":s" "me" "ma"} (s/lower-case %)))))

(defn wrong-answer [x]
  (let [ans (answer x)]
    (when (and ans (seq ans) (< 0 (count ans)))
      (if (> 1/90 (rand))
        (do
          (Thread/sleep 2000)
          ["Gave up trying to execute custom command #:3 after 1 minute because there is already one or more instances of it being executed"])
        ans))))
