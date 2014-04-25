(ns adamastor.core-test
  (:use clojure.test
        adamastor.core))

(deftest test-test-and-return
  (is (= (test-and-return odd? 7) 7))
  (is (= (test-and-return odd? 2) nil)))
