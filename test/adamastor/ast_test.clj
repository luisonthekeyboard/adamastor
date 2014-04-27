(ns adamastor.ast-test
  (:use clojure.test
        adamastor.ast))

(deftest match-horizontal-rules
  (is (= [[:rh ] ()] (horizontal-rule ["---"])))
  (is (= [[:rh ] ()] (horizontal-rule ["-  --  - -"])))
  (is (= [[:rh ] ()] (horizontal-rule ["-  -  -"])))
  (is (= [[:rh ] ()] (horizontal-rule ["***"])))
  (is (= [[:rh ] ()] (horizontal-rule ["*  **  * ** "])))
  (is (= [[:rh ] ()] (horizontal-rule ["* * *"])))
  (is (= [[:rh ] ()] (horizontal-rule ["___"])))
  (is (= [[:rh ] ()] (horizontal-rule ["_  __  _ __ "])))
  (is (= [[:rh ] ()] (horizontal-rule ["_ _ _"]))))

(deftest match-headers
  (is (= [:h1 "header 1" ()] (settext-header ["header 1" "====="])))
  (is (= [:h1 "header 1" ()] (settext-header ["header 1" "="])))
  (is (nil? (settext-header ["# header 1" "======="]))))