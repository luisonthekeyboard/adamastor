(ns adamastor.ast
  (:use [adamastor.utils]))


(defn ^:dynamic match-h1 [lines]
  (when-let [[hash text tail]
             (starts-with lines "#")]
    [[:h1 text] tail]))

(defn ^:dynamic match-li [lines]
  (when-let [[hash text tail]
             (starts-with lines "*")]
    [[:li text] tail]))

;(defn ^:dynamic horizontal-rule [lines]
;  "You can produce a horizontal rule tag (<hr />) by placing
;  three or more hyphens, asterisks, or underscores on a line
;  by themselves. If you wish, you may use spaces between
;  the hyphens or asterisks."
;  (when-let []))



(def matchers [match-li match-h1])