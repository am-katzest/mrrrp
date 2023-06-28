(ns mrrrp.gayboy)

(def bot-id  "204255221017214977")

(defn not-gayboy [id]
  (or (not= bot-id id)
      (> 0.1 (rand))))
