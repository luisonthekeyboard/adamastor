(ns adamastor.ast
  (:use [adamastor.utils])
  (:use [clojure.string :only [triml trim blank?]]))

(def line-not-preceeded-by-hash #"^(?!# ).*$")
(def line-only-with-dashes #"^-+$")
(def line-only-with-equals #"^=+$")
(def ast-header-line #"^(#+ )(.*)$")
(def ul #"^ {0,3}(\*|\+|-)( +|\t)(.+)$")
(def ol #"^ {0,3}([0-9]+\.)( +|\t)(.+)$")


(defn ^:dynamic unordered-list-item [str]
  (when-let [parts (re-matches ul str)]
    {:marker :ul
     :text (break (triml (last parts)))}))

(defn ^:dynamic ordered-list-item [str]
  (when-let [parts (re-matches ol str)]
    {:marker :ol
     :text (break (triml (last parts)))}))

(defn ^:dynamic unmarked-item [str]
  (when (and
          (not (blank? str))
          (not (matches ast-header-line str)))
    (when-let [parts (re-matches #"^(.+)$" str)]
      {:marker nil
       :text (break (triml (last parts)))})))

(defn ^:dynamic blank-item [str]
  (when (blank? str)
    {:marker nil
     :text nil}))

; (enclose :p [:li "suren" "faren" :br "meran"])
;                  ==>
;    [:li [:p "suren" "faren" :br "meran"]]
(defn ^:dynamic enclose
  "Encloses the rest of vector v in elm and then again in first of vector.
  In other words, `(enclose :p [:li \"suren\" \"faren\" :br \"meran\"])` will return
  `[:li [:p \"suren\" \"faren\" :br \"meran\"]]`. The function is smart enough to not
  do the operation in case the vector inside is already enclosed."
  [elm v]
  (if (and (vector? (second v)) (= elm (first (second v))))
    v
    (conj [(first v)] (into [elm] (rest v)))))

(defn ^:dynamic merge-item [item v]
  (if (vector? (last v))
    (conj
      (vec (drop-last v))
      (merge-item item (last v)))
    (into v item)))

(defn ^:dynamic list-item [list-items lines]
  (loop [list-items list-items
         lines lines]
    (if (empty? lines)
      list-items
      (if-let [item-as-map (some #(% (first lines)) [unordered-list-item ordered-list-item unmarked-item blank-item])]
        (cond

          (not (nil? (:marker item-as-map))) (recur (conj list-items (into [:li ] (:text item-as-map))) (rest lines))

          (not (nil? (:text item-as-map)))
            (recur
              (conj
                 (vec (drop-last list-items))
                 (merge-item (:text item-as-map) (last list-items)))
              (rest lines))

          :else
            (when-let [next-item ;; if next line is another standard item
                      (some #(% (first (rest lines))) [unordered-list-item ordered-list-item unmarked-item])]
              (recur
                (conj
                  (vec (drop-last list-items))
                  (enclose :p (last list-items))
                  (enclose :p (into [:li ] (:text next-item))))
                (drop 1 (rest lines)))))
        [list-items lines]))))


(defn ^:dynamic list-block [lines]
  "Unordered lists use asterisks, pluses, and hyphens - interchangably -
  as list markers. Ordered lists use numbers followed by periods. Tthe actual
  numbers you use to mark the list have no effect on the HTML output
  Markdown produces. List markers typically start at the left margin, but may
  be indented by up to three spaces. List markers must be followed by one
  or more spaces or a tab."
  (cond
    (matches ul (first lines)) (list-item [:ul ] lines)
    (matches ol (first lines)) (list-item [:ol ] lines)
    :else nil))


(defn ^:dynamic paragraph [lines]
  "A paragraph is simply one or more consecutive lines of text, separated by
  one or more blank lines. (A blank line is any line that looks like a
  blank line — a line containing nothing but spaces or tabs is considered
  blank.) Normal paragraphs should not be indented with spaces or tabs.
  When you do want to insert a <br /> break tag using Markdown, you end a
  line with two or more spaces, then type return."
  (loop [lines lines
         paragraph-text []]
    (if (empty? lines)
      [(into [:p ] paragraph-text) (rest lines)]
      (let [first-line (first lines) tail (rest lines)]
        (if (or
              (some (fn [pattern] (matches pattern first-line)) [ast-header-line ul ol]) ;updated it with code and blockquote later as well plus the test
              (blank? first-line))
          (if (empty? paragraph-text)
            lines
            [(into [:p ] paragraph-text) lines])
          (recur tail (into paragraph-text (break first-line))))))))


(defn ^:dynamic settext-header [lines]
  "Setext-style headers are “underlined” using equal signs (for
   first-level headers) and dashes (for second-level headers).
   Any number of underlining =’s or -’s will work. "
  (let [first-line (first lines)
        underline (second lines)
        tail (nthrest lines 2)]
    (if (not (matches line-not-preceeded-by-hash first-line))
      nil
      (cond
        (matches line-only-with-equals underline) [:h1 first-line tail]
        (matches line-only-with-dashes underline) [:h2 first-line tail]
        :else nil))))


(defn ^:dynamic atx-header [lines]
  "Atx-style headers use 1-6 hash characters at the start
  of the line,  corresponding to header levels 1-6. Optionally,
  you may “close” atx-style headers. This is pu rely cosmetic
  — you can use this if you think it looks better. The closing
  hashes don’t even need to match the number of hashes used
  to open the header. Luis: semantics for more than 6 # is that
  they are shortened to only 6."
  (let [first-line (strip-ending-hashes (first lines)) ;I would prefer to ditch `strip-ending-hashes` and do the whole regexp in `ast-header-line`
        tail (rest lines)]
    (when-let [[line hashes header]
               (re-matches ast-header-line first-line)]
      [(keyword (str "h" (min 6 (count (trim hashes))))) (trim header) tail])))


(defn ^:dynamic headers [lines]
  "Markdown supports two styles of headers, Setext and atx."
  (when-let [[header-level header-text tail]
             (some (fn [header-matcher] (header-matcher lines)) [atx-header settext-header])]
    [[header-level header-text] tail]))


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
      [[:hr ] tail]
      false)))
