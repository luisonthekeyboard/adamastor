(ns adamastor.ast-test
  (:use clojure.test
        adamastor.ast))

(deftest test-horizontal-rules
  (is (= [[:hr ] ()] (horizontal-rule ["---"])))
  (is (= [[:hr ] ()] (horizontal-rule ["-  --  - -"])))
  (is (= [[:hr ] ()] (horizontal-rule ["-  -  -"])))
  (is (= [[:hr ] ()] (horizontal-rule ["***"])))
  (is (= [[:hr ] ()] (horizontal-rule ["*  **  * ** "])))
  (is (= [[:hr ] ()] (horizontal-rule ["* * *"])))
  (is (= [[:hr ] ()] (horizontal-rule ["___"])))
  (is (= [[:hr ] ()] (horizontal-rule ["_  __  _ __ "])))
  (is (= [[:hr ] ()] (horizontal-rule ["_ _ _"]))))

(deftest test-settext-headers
  (is (= [:h1 "header 1" ()] (settext-header ["header 1" "====="])))
  (is (= [:h1 "header 1" ()] (settext-header ["header 1" "="])))
  (is (= [:h1 " # header 1" ()] (settext-header [" # header 1" "="])))
  (is (= [:h1 " #header 1" ()] (settext-header [" #header 1" "="])))
  (is (= [:h1 "#header 1" ()] (settext-header ["#header 1" "="])))
  (is (nil? (settext-header ["# header 1" "======="]))))

(deftest test-atx-headers
  (is (= [:h1 "header" ()] (atx-header ["# header"])))
  (is (= [:h2 "header" ()] (atx-header ["## header"])))
  (is (= [:h3 "header" ()] (atx-header ["### header"])))
  (is (= [:h4 "header" ()] (atx-header ["#### header"])))
  (is (= [:h5 "header" ()] (atx-header ["##### header"])))
  (is (= [:h6 "header" ()] (atx-header ["###### header"])))
  (is (nil? (atx-header ["#header 1" "======="]))))

(deftest test-headers
  (is (= [[:h1 "header"] ()] (headers ["# header"])))
  (is (= [[:h1 "header"] ()] (headers ["header" "======"])))
  (is (= [[:h2 "header"] ()] (headers ["## header"])))
  (is (= [[:h2 "header"] ()] (headers ["header" "--"])))
  (is (= [[:h3 "header"] ()] (headers ["### header"])))
  (is (= [[:h4 "header"] ()] (headers ["#### header"])))
  (is (= [[:h5 "header"] ()] (headers ["##### header"])))
  (is (= [[:h6 "header"] ()] (headers ["###### header"])))
  (is (= [[:h6 "header"] ()] (headers ["################## header"])))
  (is (nil? (headers ["##header"]))))

(deftest test-paragraph
  (is (=
        (paragraph ["here's a" " text which is nice." " \t \t\n\n " "that was a blank line"])
        [[:p "here's a" " text which is nice."] `(" \t \t\n\n " "that was a blank line")]))
  (is (=
        (paragraph ["here's a" " text which is nice.   " "and that was a <br />"])
        [[:p "here's a" " text which is nice." :br "and that was a <br />"] ()]))
  (is (= (paragraph ["*   test"]) ["*   test"]))
  (is (= (paragraph ["  - test"]) ["  - test"]))
  (is (= (paragraph ["+ test"]) ["+ test"]))
  (is (= (paragraph ["   1.\ttest"]) ["   1.\ttest"]))
  (is (= (paragraph ["## test"]) ["## test"])))

(deftest test-lists
  (is (= (list-block ["*   item"]) [:ul [:li "item"]]))
  (is (= (list-block [" -         item"]) [:ul [:li "item"]]))
  (is (= (list-block ["  +\titem"]) [:ul [:li "item"]]))
  (is (= (list-block ["   1.    item"]) [:ol [:li "item"]]))
  (is (= (list-block [" +  item" "1. another" " 34.\tanother"]) [:ul [:li "item"][:li "another"][:li "another"]]))
  (is (= (list-block ["   1. item" "  - another" "   +\tanother"]) [:ol [:li "item"] [:li "another"] [:li "another"]]))
  (is (nil? (list-block ["    * item"])))
  (is (not= (list-block ["* item" "    * item"]) [[:ul [:li "item"] [:li "item"]]]))
  (is (= (list-block ["1. item" "another  " "another" "2. final"]) [:ol [:li "item" "another" :br "another"] [:li "final"]]))
  (is (= (list-block ["* 111  " "1 e meio" "" "* 222"]) [:ul [:li [:p "111" :br "1 e meio"]] [:li [:p "222"]]]))
  (is (= (list-block ["* 111  " "1 e meio" "" "* 222" "* 333"])) [:ul [:li [:p "111" :br "1 e meio"]] [:li [:p "222"]] [:li "333"]])
  (is (= (list-block ["* 111  " "1 e meio" "" "* 222" "" "* 333"])) [:ul [:li [:p "111" :br "1 e meio"]] [:li [:p "222"]] [:li [:p "333"]]]))