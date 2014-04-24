(ns adamastor.core
  (:gen-class)
  (:use [clojure.string :only [trim split join]]))

(defn test-and-return [fn args]
  (if (fn args)
    args
    nil))

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

(defn ^:dynamic magic [lines]
  (when-let [[hash text tail]
             (starts-with lines "#")]
    [[:h1 text] tail]))

(defn ^:dynamic parse [lines]
  (when-let [[result remaining]
             (magic lines)]
    (cons result (parse remaining))))