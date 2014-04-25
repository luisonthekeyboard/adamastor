(ns adamastor.core
  (:gen-class)
  (:use [adamastor.ast]))

; to be deleted
(defn test-and-return [fn args]
  (if (fn args)
    args
    nil))

(defn ^:dynamic parse [lines]
  (when-let [[result remaining]
             (some (fn [matcher] (matcher lines)) matchers)]
    (cons result (parse remaining))))
