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
                        ["m+r*[ieao]*[ea]+[ieao]*[łwu]*[eao]*"
                         "m+r+[ieao]+[łwu]+"
                         "m+[oae]?r*[łwu]*"
                         "p+u*rr+"
                         "m[nmpwreaouwi]+[włu]" ; this one should get most
                         (str "r+" (at-least-one "[nmwreaouwi]" "[rea]") "[iawłu]+")
                         "m+r+p+"
                         "m+r+a+"
                         "m+r+i+a+p+"
                         "m+rr+"])
                ")"))
(def nyas #"(n[- ])?n+[mnyi]+a+")
(def junk #"[^\p{L}0-9]")
(def single-meowgex (re-str "(?i)(" meows "|" nyas ")"))
(def single-meowgex-with-junk (re-str junk "*" single-meowgex junk "*"))
(def multi-meowgex (re-str single-meowgex "(" junk "+" single-meowgex ")*"))
(def meowgex (re-str "^ *" multi-meowgex "[?!~ ]*$"))

;╻ ╻   ╻ ╻
                                        ;┃ ┃┃ ┃┃ ┃
                                        ;┗━┛┗┻┛┗━┛

(def catfaces ["uwu" ":3" "!!"])

(def just-catface #"(?i)((.)[wvo_-]\2|>?:3c?|:.|[^\p{L}0-9\s]{2,})")
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

(defn extract-meows [input]
  [(strip-trailing-catface input)])

(defn ^:dynamic append-catface [meow]
  (str meow " " (rand-nth catfaces)))

(defn answer [input]
  (b/cond
    :let [s (strip-trailing-catface input)]
    (re-pred single-meowgex input) [(append-catface input)] ; meow -> meow :3
    (re-pred single-meowgex-with-junk input) [input]        ; meow! -> meow!
    (re-pred single-meowgex s) [(append-catface s)] ; meow :3 -> meow ^w^
    (re-pred single-meowgex-with-junk s) [(append-catface s)]        ; meow! :3 -> meow!
    ))
(answer "meow!!!~ :3")
