(ns mrrrp.meowperhonsion
  (:require [clojure.string :as s]))

(defn any-order [a b]
  (str "(" a b "|" b a ")"))
(defn at-least-one [main single]
  (str "("
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
                         "m[nmwreaouwi]+[włu]" ; this one should get most
                         (str "r+" (at-least-one "[nmwreaouwi]" "[rea]") "[iawłu]+")
                         "m+r+p+"
                         "m+r+a+"
                         "m+r+i+a+p+"
                         "m+rr+"])
                ")"))
(def nyas "(n[- ])?n+[mnyi]+a+")
(def single-meowgex (str "(" meows "|" nyas ")"))
(def multi-meowgex (str single-meowgex "([^a-zA-Z0-9]+" single-meowgex ")*"))
(def meowgex (str "^ *" multi-meowgex "[?!~ ]*$"))

;╻ ╻   ╻ ╻
;┃ ┃┃ ┃┃ ┃
;┗━┛┗┻┛┗━┛
(def catface #"\s*(?i)((.)[wvo_-]\2|>?:3c?|:.|[^a-z0-9\s])\s*")

(defn strip [x]
  (-> x
      (s/replace  catface " ")
      (s/replace #"^\s*" "")
      (s/replace #"\s*$" "")))

(defn extract-meows [input]
  [(strip input)])

(defn answer [input]
  (extract-meows input))
