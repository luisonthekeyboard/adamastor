(ns adamastor.utils-test
  (:use clojure.test
        adamastor.utils))

(deftest test-get-text-from-file
  (is (not (nil? (get-text-from-file)))))

(deftest test-remove-whitespaces
  (is (= "token" (remove-whitespaces "   to   ken  \t ")))
  (is (= "token" (remove-whitespaces "   to \n  ken")))
  (is (= "token" (remove-whitespaces "to   ken \n  ")))
  (is (= "" (remove-whitespaces "      "))))

(deftest test-only-contains
  (is (true? (only-contains "abbaabbabbbaaababbbbababbababbaaaba" "ab")))
  (is (true? (only-contains "aaaaaaaaaaaaaaa" "a")))
  (is (true? (only-contains "aaaaaaaaaaaaaaa" \a)))
  (is (true? (only-contains "" "")))
  (is (true? (only-contains "ab" "abbabababbababbaabba")))
  (is (false? (only-contains "" "ab")))
  (is (false? (only-contains "ab" ""))))

(deftest test-strip-ending-hashes
  (is (= (strip-ending-hashes "abbaabb") "abbaabb"))
  (is (= (strip-ending-hashes "###### abbaabb") "###### abbaabb"))
  (is (= (strip-ending-hashes "abbaabb #########") "abbaabb"))
  (is (= (strip-ending-hashes "abbaabb#########") "abbaabb#########"))
  )