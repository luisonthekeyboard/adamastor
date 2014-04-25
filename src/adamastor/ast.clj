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

(defn ^:dynamic horizontal-rule [lines]
  "You can produce a horizontal rule tag (<hr />) by placing
  three or more hyphens, asterisks, or underscores on a line
  by themselves. If you wish, you may use spaces between
  the hyphens or asterisks."
  (let [hr-line (first lines) tail (rest lines)]
    (if
      (some
        (fn [character]
          (and
            (only-contains (remove-whitespaces hr-line) character)
            (>= (get (frequencies hr-line) character) 3)))
        [\- \_ \*])
      [[:rh ] tail]
      false)))

(def matchers [match-li match-h1])