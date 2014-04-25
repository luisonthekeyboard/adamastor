(ns adamastor.utils
  (:use [clojure.string :only [trim split join]])
  (:import java.lang.Character))


(defn get-text-from-file []
  (clojure.string/split-lines
    (slurp "./resources/test.md")))


(defn ^:dynamic tokenize [line]
  (trim (join " " (rest (split line #" ")))))


(defn ^:dynamic starts-with [lines starter]
  (if (empty? lines)
    nil
    (if (.startsWith (first lines) starter)
      [starter (tokenize (first lines)) (into [] (rest lines))]
      nil)))

(defn ^:dynamic remove-whitespaces
  "Takes a string as an input and returns a copy of the string with all
  the whitespaces removed. A whitespace is defined according
  to java.lang.Character.isWhitespace. Actually, that's the method I'm using."
  [string]
  (loop [string string
         result ""]
    (let [head (first string)
          tail (rest string)]
      (if (nil? head)
        result
        (recur tail (str
                      result
                      (if-not (java.lang.Character/isWhitespace head) head nil)))))))

(defn ^:dynamic only-contains
  "Takes a string and a sequence of characters and returns true if the string
  composed only of those characters. Admissable characters can be either a string
  or a Character."
  [string admissable-characters]
  (= (set string) (set (str admissable-characters))))