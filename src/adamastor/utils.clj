(ns adamastor.utils
  (:use [clojure.string :only [trim triml trimr split join blank?]])
  (:import java.lang.Character))

(def hash-ending-string #"^(.*) (#+)$")
(def enclosable [:p ])
(def blockquote-line #"^ {0,3}>( (.*))?$")
(def indented-line #"^\t(.+)$")

(defn ^:dynamic strip-ending-hashes [string]
  "Takes a string (possibly) ending with /space hash/ and returns a copy
   without that ending."
  (if-not (string? string)
    string
    (if-let [[whole-string minus-hashes hashes]
             (re-matches hash-ending-string string)]
      minus-hashes
      string)))


(defn ^:dynamic matches [regexp string]
  "Checks if a regular expression matches a string.
   Will return true or false."
  (cond
    (nil? regexp) false
    (nil? string) false
    :else (not (nil? (re-matches regexp string)))))


(defn break [string]
  (if (matches #"^(.*)(  )$" string)
    [(trimr string) :br ]
    [(trimr string)]))


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


(defn ^:dynamic enclose
  "Encloses the rest of vector v in elm and then again in first of vector.
  In other words, `(enclose :p [:li \"suren\" \"faren\" :br \"meran\"])` will return
  `[:li [:p \"suren\" \"faren\" :br \"meran\"]]`. The function is smart enough to not
  do the operation in case the vector inside is already enclosed."
  [elm v]
  (if (and (vector? (second v)) (= elm (first (second v))))
    v
    (conj [(first v)] (into [elm] (rest v)))))

(defn ^:dynamic same-head [v1 v2]
  (and
    (vector? v1)
    (vector? v2)
    (= (first v1) (first v2))))

(defn ^:dynamic merge-item [v1 v2]
  (into (into [(first v1)] (rest v1)) (rest v2)))

(defn ^:dynamic is-enclosing [v]
  (some? (some (fn [element] (and (vector? v) (= (first v) element))) enclosable)))

(defn ^:dynamic merge-enclosable [v]
  (if (and (is-enclosing (second (reverse v))) (string? (last v)))
    (conj (vec (drop-last 2 v)) (conj (second (reverse v)) (last v)))
    v))

(defn ^:dynamic mark-with [key v]
  (if (= (second v) key)
    v
    (into (conj [] (first v) key) (vec (rest v)))))

(defn ^:dynamic add-element [item v]
  (cond
    (= 1 (count v)) (conj v item)
    (or (string? (last v)) (keyword? (last v))) (conj v item)
    (vector? (last v))
      (if (and (vector? item) (same-head (last v) item))
        (conj (vec (drop-last v)) (merge-enclosable (merge-item (last v) item)))
        (conj (vec (drop-last v)) (add-element item (last v))))))

(defn ^:dynamic strip-starting-quote [string]
  (if (matches blockquote-line string)
    (join (drop 2 (triml string)))
    string))

(defn ^:dynamic strip-starting-quotes [lines]
  (loop [stripped []
         lines lines]
    (if (or (empty? lines) (every? blank? (take 2 lines)))
      [stripped (vec lines)]
      (recur (conj stripped (strip-starting-quote (first lines))) (rest lines)))))

(defn ^:dynamic unindent-line [line]
  (if-let [match (re-matches indented-line line)]
    (second match)
      line))

(defn ^:dynamic unindent-lines [lines]
  (loop [processed []
          remaining lines]
    (if (or (empty? remaining) (every? blank? (take 2 remaining)))
      [processed (vec remaining)]
      (recur (conj processed (unindent-line (first lines))) (rest lines)))))