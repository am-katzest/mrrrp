(ns mrrrp.meowperhonsion
  (:require [clojure.string :as s]
            [better-cond.core :as b]))

(def re-str (comp re-pattern str))

(defn matches? [pred x] (some? (re-matches pred x)))

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
                        ["m+r*[yieao]*[ea]+[ieao]+[łwu]*[eao]*"
                         "m+r+[ieao]+[łwu]+"
                         "m+[oae]+r+[łwu]*"
                         "m+[oae]+r*[łwu]+"
                         '"p+u*rr+"
                         "m[ynmpwreaouwi]+[włu]" ; this one should get most
                         (str "r+" (at-least-one "[nmwreaouwi]" "[rea]") "[iawłu]+")
                         "m+r+(p+|e+|a+)"
                         "m+r+i+a+p+"
                         "m+y*a+o*w+"
                         "m+y*a+o+w*"
                         "m+y+a+o+w*"
                         "m+rr+"])
                ")"))

(def nyas #"(n[- ])?n+[mnyi]+a+")

(def  woofs (str #"([wa][- ])" (s/join "|"
                                       ["w+o+[ao]+f*"
                                        "a+w+(u+|o+)f*"
                                        "[ar]*w+r*(u+|o+)f+"
                                        "a+r+f+"
                                        "r+(o+|u+)f+"
                                        "p+a+o+r*"
                                        "p+o+a+r*"
                                        "bj*+a+r+kf*"
                                        "bj*+o+r+kf*"])))

(def obvious-meows #"nya|mrr+p?|mraow|meow")
(def obvious-woofs #"woof|bark|awo+")
(def obvious-catfaces #"uwu|owo|:3|^w^|B3")

(def junk #"[^\p{L}0-9]")
(def woofgex (re-str "(?i)(" woofs ")"))
(def meowgex (re-str "(?i)(" meows "|" nyas ")"))

(defn tolerate-junk [re] (re-str junk "*" re junk "*"))
(def meowgex-with-junk (tolerate-junk meowgex))

(def woofgex-with-junk (tolerate-junk woofgex))

(defn multi [re] (re-str "^ *"  re "(" junk "+" re ")*"  "[?!~ ]*$"))

(def multi-meowgex (multi meowgex))
(def multi-woofgex (multi woofgex))

(def catfaces [">:3" ":p" "OwO"])
(def just-catface #"(?i)(([^\p{Alpha}\s])[wvo_-]\2|(>|<\ ])?:3c?|:[p3>\])]|uwu|owo)")
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
                 (s/replace meowgex ""))]
    (matches? #"(?i)^[^\p{L}0-9]*$" bare)))

(defn extract-meows [input]
  [(strip-trailing-catface input)])

(defn ^:dynamic append-catface
  ([meow]
   (str meow " " (rand-nth catfaces)))
  ([chance meow]
   (if (> chance (rand))
     (append-catface meow)
     meow)))

(def some-stuff (re-str #"\b" just-catface #"\b"
                        "|" obvious-catfaces
                        "|" #"\b" meowgex #"\b"
                        "|" #"\b" woofgex #"\b"
                        "|" obvious-meows
                        "|" obvious-woofs))

(defn dig-for-stuff [x]
  (let [ans (->> x
                 (re-seq some-stuff)
                 (map first))]
    (when (pos? (count ans)) ans)))

(defn make-woofs
  ([]
   (rand-nth ["woof" "Woof" "bark" "bark!" "BARK" "arf" "arf!" "yep" "ruff" "ruff!" "roff" "rwoff" "freh" "woof" "bark" "awooooo"]))
  ([n]
   (s/join " " (repeatedly n make-woofs))))

(defn make-meows
  "produces several meows"
  ([]
   (rand-nth ["meow" "Meow" "meow!" "mriaa" "mriaw!" "mrie" "m mraow?" "mrow" "mrrrrrp" "MEEEEOOOOWWW!" "MIAU"]))
  ([n]
   (s/join " " (repeatedly n make-meows))))

(defn answer [input & {:keys [cooperate]}]
  (->>
   (b/cond
     :let [s (strip-trailing-catface input)]
     (= "*meows*" s) ["*meows back*"]
     (= "*barks*" s) ["*barks back*"]
     (= "*hiss*" s) ["m-meow? ;-;"]
     ;; single meows
     (matches? meowgex input) [(append-catface 0.2 input)] ; meow -> meow :3
     (matches? meowgex-with-junk input) [input]            ; meow! -> meow!
     (matches? meowgex s) [(append-catface s)]             ; meow :3 -> meow ^w^
     (matches? meowgex-with-junk s) [(append-catface s)] ; meow! :3 -> meow! ^w^

     ;; repsond with meows to woofs
     (matches? woofgex input) [(append-catface 0.2 input)]
     (matches? woofgex-with-junk input) [input]
     (matches? woofgex s) [(append-catface s)]
     (matches? woofgex-with-junk s) [(append-catface s)]

     ;; just meows
     (matches? multi-meowgex input) [(append-catface 0.4 s)]
     (matches? multi-meowgex s) [(append-catface s)]
     (matches? multi-woofgex input) [(append-catface 0.4 s)]
     (matches? multi-woofgex s) [(append-catface s)]

     ;; meows mixed with catfaces
     (and (re-seq multi-meowgex input) (just-cat-stuff? input)) [(append-catface s)]
     (and (re-seq multi-woofgex input) (just-cat-stuff? input)) [(append-catface s)]

     ;; just catface
     (matches? just-catface input) [input]

     ;; dig meows
     :let [assorted-meows (dig-for-stuff input)]
     (some? assorted-meows) (take 1 assorted-meows)
     [])))

(defn wrong-answer [x]
  (let [ans (answer x)]
    (when (and ans (seq ans) (< 0 (count ans)))
      (if (> 1/900 (rand))
        (do
          (Thread/sleep 2000)
          ["Gave up trying to execute custom command #:3 after 1 minute because there is already one or more instances of it being executed"])
        ans))))

(defn maybe-meow-back [text reply-fn!]
  (when-let [answer (wrong-answer text)]
    (doseq [ans answer] (reply-fn! ans))))

;; (->> "/etc/dictionaries-common/words" slurp s/split-lines (filter #(re-matches meowgex-with-junk %)) (map println))
