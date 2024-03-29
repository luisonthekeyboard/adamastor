(ns adamastor.utils-test
  (:use clojure.test
        adamastor.utils))

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
  (is (= (strip-ending-hashes "abbaabb#########") "abbaabb#########")))

(deftest test-same-head
  (is (true? (same-head [:li "suren"] [:li "faren"])))
  (is (false? (same-head [:li "suren"] ["faren" :li]))))

(deftest test-merge
  (is (= (merge-item [:li "suren" "faren"] [:l1 "guren"])
        [:li "suren" "faren" "guren"]))
  (is (= (merge-item [:li "faren"] [:l1 "guren" [:p "meran"]])
        [:li "faren" "guren" [:p "meran"]])))

(deftest test-is-enclosing
  (is (true? (is-enclosing [:p "suren"])))
  (is (true? (is-enclosing [:p "suren" "faren"])))
  (is (false? (is-enclosing [:faren "suren" "faren"])))
  (is (false? (is-enclosing ["suren" "faren"])))
  (is (false? (is-enclosing "suren")))
  (is (false? (is-enclosing :guren))))

(deftest test-merge-enclosable
  (is (= (merge-enclosable [:li [:p "suren"] "faren"])
        [:li [:p "suren" "faren"]]))
  (is (= (merge-enclosable [:li [:p "suren" :br "meran"] "faren"])
        [:li [:p "suren" :br "meran" "faren"]]))
  (is (= (merge-enclosable [:li [:h1 "suren"] "faren"])
        [:li [:h1 "suren"] "faren"])))

(deftest test-strip-starting-quote
  (is (= (strip-starting-quote "   > #luis") "#luis"))
  (is (= (strip-starting-quote "    > #luis") "    > #luis")))

(deftest test-strip-starting-quotes
  (is (= (strip-starting-quotes ["> test" ">   suren" "   > faren" "> 1. guren" "> ## meran"])
        [["test" "  suren" "faren" "1. guren" "## meran"] []]))
  (is (= (strip-starting-quotes ["" "" "> test" ">   suren" "   > faren" "> 1. guren" "> ## meran"])
        [[]["" "" "> test" ">   suren" "   > faren" "> 1. guren" "> ## meran"]]))
  (is (= (strip-starting-quotes ["> test" ">   suren" "   > faren" "\n\t" "     " "> 1. guren" "> ## meran"])
        [["test" "  suren" "faren"] ["\n\t" "     " "> 1. guren" "> ## meran"]]))
  (is (= (strip-starting-quotes ["> > ## meran"])
        [["> ## meran"] []])))

(deftest test-unindent-line
  (is (= (unindent-line "\t\tluis") ) "\tluis")
  (is (= (unindent-line "\t> luis") ) "> luis")
  (is (= (unindent-line "\t") ) "\t")
  (is (= (unindent-line "lalala") ) "lalala"))

;(deftest test-unindent-lines
;  (is (= (unindent-lines ["luis"]) [["luis"] []]))
;  (is (= (unindent-lines ["\tluis"]) [["luis"]
;    adamastor.core=> (unindent-lines ["\tluis" "\t"suren" "\t faren"])
;            #_=>
;
;adamastor.core=> (unindent-lines ["\tluis" "\t suren" "\t faren"])
;["luis" " suren" " faren"]
;adamastor.core=> (unindent-lines ["\tluis" "\t suren" "\t faren" "" "" "\t suren"])
;["luis" " suren" " faren" "" "" " suren"]
