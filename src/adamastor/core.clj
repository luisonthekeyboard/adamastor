(ns adamastor.core
  (:gen-class)
  (:use [adamastor.ast]))

; to be deleted
(defn test-and-return [fn args]
  (if (fn args)
    args
    nil))


