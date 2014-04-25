(ns adamastor.ast-test
  (:use clojure.test
        adamastor.ast))

(deftest match-horizontal-rules
  (is (= [[:rh] ()] (horizontal-rule ["---"])))
  (is (= [[:rh] ()] (horizontal-rule ["-  --  - -"])))
  (is (= [[:rh] ()] (horizontal-rule ["-  -  -"])))
  (is (= [[:rh] ()] (horizontal-rule ["***"])))
  (is (= [[:rh] ()] (horizontal-rule ["*  **  * ** "])))
  (is (= [[:rh] ()] (horizontal-rule ["* * *"])))
  (is (= [[:rh] ()] (horizontal-rule ["___"])))
  (is (= [[:rh] ()] (horizontal-rule ["_  __  _ __ "])))
  (is (= [[:rh] ()] (horizontal-rule ["_ _ _"]))))

