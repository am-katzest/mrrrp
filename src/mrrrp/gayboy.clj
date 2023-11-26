(ns mrrrp.gayboy
  (:require
   [clojure.tools.logging :as log]
   [mrrrp.meowperhonsion :as re]))

;; it's called yagpdb or something, but on one server it's named "gayboy" so i remember it as such
(def gayboy-meow (re/re-str "^" re/meowgex "$"))
#_(str gayboy-meow)


(defn is-gayboy-able-to-handle-this-message? [msg]
  (or (re/matches? gayboy-meow msg)
      (re/matches? #".*(:3|OwO|\^w\^|UwU).*" msg)))



(defn author-is-gayboy? [context]
  (contains? (-> context :config :gayboy :id)
             (-> context :message :author)))

(def update-gayboy-interceptor
  {:enter
   (fn [{:keys [message] :as context}]
     (if (and (author-is-gayboy? context)
              (is-gayboy-able-to-handle-this-message? (:content message)))
       (update-in context [:state :gayboy-channels] conj (:channel message))
       context))})

(def ignore-gayboy-interceptor
  {:enter
   (fn [{:keys [message config] :as context}]
     (if (and (contains? (-> context :state :gayboy-channels) (:channel message))
              (is-gayboy-able-to-handle-this-message? (:content message)))
       (if (and (author-is-gayboy? context)
                (> (-> config :gayboy :meowback-chance) (rand)))
         context
         (assoc context :stop true))
       context))})
