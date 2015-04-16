(defproject adamastor "0.2.0"
  :description "Destroying caravels since the 1400s..."
  :url "https://github.com/decomputed/adamastor"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :main adamastor.core
  :aot [adamastor.core]
  :dependencies [ [org.clojure/clojure "1.6.0"]
                  [org.clojure/tools.trace "0.7.8"] ])
