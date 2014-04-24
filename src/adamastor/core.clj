(ns adamastor.core
  (:gen-class))

(defn test-and-return [fn args]
  (if (fn args)
    args
    nil))