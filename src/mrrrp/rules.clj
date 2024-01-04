(ns mrrrp.rules
  (:require [malli.core :as m]))

(defn- str-set-limiter [name]
  [name {:optional true} [:set :string]])

(defn regex? [thing]
  (isa? (type thing) (key (first (m/class-schemas)))))

(def condition-schema
  (m/schema
   [:map
    [:text [:fn regex?]]
    (str-set-limiter :only-channels)
    (str-set-limiter :only-users)
    (str-set-limiter :only-servers)]))

(def action-schema (m/schema [:tuple [:= :reply] string?]))

(def rule-schema
  (m/schema [:map
             [:condition condition-schema]
             [:action action-schema]]))
