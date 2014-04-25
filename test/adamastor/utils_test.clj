(ns adamastor.utils-test
  (:use clojure.test
        adamastor.utils))

(deftest test-get-text-from-file
  (is (not (nil? (get-text-from-file)))))

(deftest test-tokenize
  (is (= "token" (tokenize "# token")))
  (is (= "token token token" (tokenize "# token token token")))
  (is (= "token token token" (tokenize "# token token token   "))))