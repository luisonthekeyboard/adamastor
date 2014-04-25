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

(def matchers [match-li match-h1])