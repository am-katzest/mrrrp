(defproject mrrrp "0.2.0-SNAPSHOT"
  :description "it meows"
  :license {:name "AGPL-3.0-or-later"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [better-cond "2.1.5"]
                 [clj-statecharts "0.1.7"]
                 [org.suskalo/discljord "1.1.1"]
                 [aero "1.1.6"]
                 [metosin/malli "0.13.0"]]
  :repl-options {:init-ns mrrrp.core}
  :uberjar-name "mrrrp.jar"
  :main mrrrp.core)
