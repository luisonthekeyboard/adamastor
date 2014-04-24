(ns adamastor.core
  (:gen-class))

(defn test-and-return [fn args]
  (if (fn args)
    args
    nil))

(defn get-text-from-file []
  (clojure.string/split-lines
    (slurp "./resources/test.md")))