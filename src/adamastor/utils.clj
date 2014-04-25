(ns adamastor.utils
  (:use [clojure.string :only [trim split join]]))


(defn get-text-from-file []
  (clojure.string/split-lines
    (slurp "./resources/test.md")))


(defn ^:dynamic tokenize [line]
  (trim (join " " (rest (split line #" ")))))


(defn ^:dynamic starts-with [lines starter]
  (if (empty? lines)
    nil
    (if (.startsWith (first lines) starter)
      [starter (tokenize (first lines)) (into [](rest lines))]
      nil)))