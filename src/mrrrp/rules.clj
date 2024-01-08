(ns mrrrp.rules
  (:require [malli.core :as m]
            [mrrrp.meowperhonsion :as re]))

(defn- str-set-limiter [name]
  [name {:optional true} [:set :string]])

(def condition-schema
  (m/schema
   [:map
    [:text :string]
    (str-set-limiter :only-channels)
    (str-set-limiter :only-authors)
    (str-set-limiter :only-servers)]))

(def action-schema (m/schema [:tuple [:= :reply] string?]))

(def rule-schema
  (m/schema [:map
             [:condition condition-schema]
             [:action action-schema]]))

(defn applies? [context rule]
  (every? (fn [[key value]]
            (case key
              :only-channels (-> context :message :channel value)
              :only-authors (-> context :message :author value)
              :only-servers (-> context :event :guild-id value)
              :text (->> context :message :content (re-matches (re-pattern value)))))
          (:condition rule)))

(defmulti action first)

(defmethod action :reply
  [[_ text]]
  [:reply text])

(defn actions [context rule]
  (when (applies? context rule)
    [(action (:action rule))]))

(def apply-rules-interceptor
  {:enter (fn [context]
            (let [rules (-> context :config :rules)
                  actions (mapcat #(actions context %) rules)]
              (if (empty? actions)
                context
                (assoc context :fx actions))))})
