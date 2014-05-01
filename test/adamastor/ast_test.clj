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
  (is (= (list-block ["* 111  " "1 e meio" "" "* 222"])
         [:ul :p [:li "111" :br "1 e meio"] [:li "222"]]))
  (is (= (list-block ["* 111  " "1 e meio" "" "* 222" "* 333"]))
         [:ul :p [:li "111" :br "1 e meio"] [:li "222"] [:li "333"]])
  (is (= (list-block ["* 111  " "1 e meio" "" "* 222" "" "* 333"]))
         [:ul :p [:li "111" :br "1 e meio"] [:li "222"] [:li "333"]])
  (is (= (list-block ["* 111" "222" "333" "        " "444" "555" "\t\t\t\t\t" "* 666"])
         [:ul :p [:li "111" "222" "333"] [:li "444" "555"] [:li "666"]]))
  (is (= (list-block ["* 111" "\t222" "\t333" "\t35353535" "" "444" "555" "" "* 666"])
          [:ul :p [:li "111" "222" "333" "35353535"] [:li "444" "555"] [:li "666"]]))
  (is (= (list-block ["* 111" "\t222" "\t333" "\t35353535" "" "444" "555" "" "* 666" "*different thing"])
         [:ul :p [:li "111" "222" "333" "35353535"] [:li "444" "555"] [:li "666" "*different thing"]]))
  (is (= (list-block ["* 111" "\t222" "\t333" "\t35353535" "" "444" "555" "" "* 666" "* different thing"])
         [:ul :p [:li "111" "222" "333" "35353535"] [:li "444" "555"] [:li "666"] [:li "different thing"]])))

(deftest test-blockquotes
  (is (= (blockquote ["   > luis" "bipi"]) [:blockquote [:qi "luis" "bipi"]]))
  (is (= (blockquote ["   > luis" "> bipi"]) [:blockquote [:qi "luis" "bipi"]]))
  (is (= (blockquote ["   > luis" ">bipi"]) [:blockquote [:qi "luis" "bipi"]]))
  (is (= (blockquote ["> luis" "bipi" "" "> teofilo"])
         [:blockquote :p [:qi "luis" "bipi"] [:qi "teofilo"]]))
  (is (= (blockquote ["> luis" "bipi" "" "> teofilo" "nothing" "under" "the sun" "       " "> and one more"])
      [:blockquote :p [:qi "luis" "bipi"] [:qi "teofilo" "nothing" "under" "the sun"] [:qi "and one more"]]))
  (is (= (blockquote ["> luis" "bipi" "> teofilo" "nothing" "under" "the sun" "> and one more"])
          [:blockquote [:qi "luis" "bipi" "teofilo" "nothing" "under" "the sun" "and one more"]]))
  (is (= (blockquote ["> luis" "bipi" "> teofilo" "nothing  " "under  " "the sun" "> and one more"])
         [:blockquote [:qi "luis" "bipi" "teofilo" "nothing" :br "under" :br "the sun" "and one more"]]))
  (is (= (blockquote ["> test" "### mega suren" "faren" "meran"])
        [:blockquote [:qi "test" [:h3 "mega suren" ()] "faren" "meran"]]))
  (is (= (blockquote ["> luis  " "bipi" "# teofilo"]))
    [:blockquote [:qi "luis" :br "bipi" [:h1 "teofilo" ()]]])
  (is (= (blockquote ["> test" "### mega suren" "faren  " "> meran"])
      [:blockquote [:qi "test" [:h3 "mega suren" ()] "faren" :br "meran"]]))
  (is (= (blockquote ["> luis" "bipi" "teofilo" "" "under" "the sun" "and one more" "" "and" "finaly"])
        [:blockquote :p [:qi "luis" "bipi" "teofilo"] [:qi "under" "the sun" "and one more"] [:qi "and" "finaly"]]))
  (is (= (blockquote ["> # title" "" "> In the beggining the was nothing  " "Then John created Lisp" "> ## title 2" "Then came Rich and created Clojure" "" "> And Rich was happy"])
        [:blockquote :p [:qi [:h1 "title" ()]] [:qi "In the beggining the was nothing" :br "Then John created Lisp" [:h2 "title 2" ()] "Then came Rich and created Clojure"] [:qi "And Rich was happy"]]))
  (is (= (blockquote ["> luis" "> bipi" "> " "> "])
        [:blockquote [:qi "luis" "bipi"]]))
  (is (= (blockquote ["> luis" "> bipi" "> " "> teofilo" "> popi"])
        [:blockquote :p [:qi "luis" "bipi"] [:qi "teofilo" "popi"]]))
  (is (= (blockquote ["> title" "" "> In the beggining the was nothing  " "Then John created Lisp" "> title 2" "Then came Rich and created Clojure" "" "And Rich was happy"])
        [:blockquote :p [:qi "title"] [:qi "In the beggining the was nothing" :br "Then John created Lisp" "title 2" "Then came Rich and created Clojure"] [:qi "And Rich was happy"]]))
  )

(blockquote [
              "> # title"
              ""
              "> In the beggining the was nothing  "
              "Then John created Lisp"
              "> ## title 2"
              "Then came Rich and created Clojure"
              ""
              "> And Rich was happy"])


[:blockquote
 :p
 [:qi
  [:h1 "title" ()]]

 [:qi
  "In the beggining the was nothing"
  :br
  "Then John created Lisp"
  [:h2 "title 2" ()]
  "Then came Rich and created Clojure"]

 [:qi "And Rich was happy"]]