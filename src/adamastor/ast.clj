(ns adamastor.ast
  (:use [adamastor.utils])
  (:use [clojure.string :only [triml trim blank?]]))

(declare parse paragraph list-block blank-line horizontal-rule headers)

(def line-not-preceeded-by-hash #"^(?!# ).*$")
(def line-only-with-dashes #"^-+$")
(def line-only-with-equals #"^=+$")
(def ast-header-line #"^(#+ )(.*)$")
(def ul #"^ {0,3}(\*|\+|-)( +|\t)(.+)$")
(def ol #"^ {0,3}([0-9]+\.)( +|\t)(.+)$")

(defn ^:dynamic standard-blockquote [str]
  (when-not (= ">" (trim str))
    (when-let [parts (re-matches blockquote-line str)]
      {:text (break (triml (second parts)))})))

(defn ^:dynamic unmarked-blockquote [str]
  (when-not (= ">" (trim str))
    (when (not (blank? str))
      (when-let [parts (re-matches #"^(.+)$" str)]
        {:text (break (triml (last parts)))}))))

(defn ^:dynamic blank-line-blockquote [str]
  (when (or (blank? str) (= ">" (trim str)))
    {:text nil}))

(defn ^:dynamic parse-text [text]
  (if-let [parsed (some #(% text) [atx-header])]
    [parsed]
    text))


(defn ^:dynamic blockquote-item [quoted-items lines]
  (loop [quoted-items quoted-items
         lines lines]
    (if (empty? lines)
      quoted-items
      (if-let [item-as-map (some #(% (first lines)) [standard-blockquote unmarked-blockquote blank-line-blockquote])]

        (cond
          (not (nil? (:text item-as-map)))
          (recur
            (add-element (into [:qi ] (parse-text (:text item-as-map))) quoted-items)
            (rest lines))

          :else ;we have ourselves a blank line
          (if (nil? (first (rest lines)))
            quoted-items
            (if-let [next-item ;; if next line is another standard item
                     (some #(% (first (rest lines))) [standard-blockquote unmarked-blockquote])]
              (recur
                (conj
                  (mark-with :p quoted-items)
                  (into [:qi ] (:text next-item)))
                (drop 1 (rest lines)))
              quoted-items)))))))

(defn ^:dynamic blockquote [lines]
  "Markdown uses email-style > characters for blockquoting. It looks best if you
  hard wrap the text and put a > before every line. Markdown allows you to be lazy
  and only put the > before the first line of a hard-wrapped paragraph. Blockquotes
  can be nested (i.e. a blockquote-in-a-blockquote) by adding additional levels of >.
  Blockquotes can contain other Markdown elements, including headers, lists, and
  code blocks."
  (when (matches blockquote-line (first lines))
    (let [[blockquote-items tail]
          (strip-starting-quotes lines)]
      [(conj [:blockquote ] (parse blockquote-items)) tail])))

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

(defn ^:dynamic double-blank [lines]
  (every? blank? (take 2 lines)))

(defn ^:dynamic list-item [list-items lines]
  (loop [list-items list-items
         lines lines]
    (if (or (empty? lines) (every? blank? (take 2 lines)))
      [list-items lines]
      (if-let [item-as-map
               (some #(% (first lines)) [unordered-list-item ordered-list-item unmarked-item blank-item])]
        (cond
          (not (nil? (:marker item-as-map)))
          (recur (conj list-items (into [:li ] (:text item-as-map))) (rest lines))
          (not (nil? (:text item-as-map)))
          (recur
            (add-element (into [:li ] (:text item-as-map)) list-items)
            (rest lines))
          :else (if-let [next-item ;; if next line is another standard item
                         (some #(% (first (rest lines))) [unordered-list-item ordered-list-item unmarked-item])]
                  (recur
                    (conj
                      (mark-with :p list-items)
                      (into [:li ] (:text next-item)))
                    (drop 1 (rest lines)))
                  list-items))
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

(defn ^:dynamic text [lines]
  (and
    (not (empty? lines))
    (nil? (some (fn [matcher] (matcher lines)) [blank-line horizontal-rule headers list-block]))))

(defn ^:dynamic paragraph
  "A paragraph is simply one or more consecutive lines of text, separated by
  one or more blank lines. (A blank line is any line that looks like a
  blank line — a line containing nothing but spaces or tabs is considered
  blank.) Normal paragraphs should not be indented with spaces or tabs.
  When you do want to insert a <br /> break tag using Markdown, you end a
  line with two or more spaces, then type return."
  [lines]
  (when (and (coll? lines) (text (take 2 lines)))
    (loop [paragraph-text (break (first lines))
           remaining (rest lines)]
      (if (text remaining)
        (recur (into paragraph-text (break (first remaining))) (rest remaining))
        [(into [:p ] paragraph-text) remaining]))))


(defn ^:dynamic settext-header
  "Setext-style headers are “underlined” using equal signs (for
   first-level headers) and dashes (for second-level headers).
   Any number of underlining =’s or -’s will work. "
  [lines]
  (when (coll? lines)
    (let [first-line (first lines)
          underline (second lines)
          tail (nthrest lines 2)]
      (if (not (matches line-not-preceeded-by-hash first-line))
        nil
        (cond
          (matches line-only-with-equals underline) [:h1 first-line tail]
          (matches line-only-with-dashes underline) [:h2 first-line tail]
          :else nil)))))

(defn ^:dynamic atx-header [lines]
  "Atx-style headers use 1-6 hash characters at the start
  of the line,  corresponding to header levels 1-6. Optionally,
  you may “close” atx-style headers. This is purely cosmetic
  — you can use this if you think it looks better. The closing
  hashes don’t even need to match the number of hashes used
  to open the header. Luis: semantics for more than 6 # is that
  they are shortened to only 6."
  (when (coll? lines)
    (let [first-line (strip-ending-hashes (first lines)) ;I would prefer to ditch `strip-ending-hashes` and do the whole regexp in `ast-header-line`
          tail (rest lines)]
      (when-let [[line hashes header]
                 (re-matches ast-header-line first-line)]
        [(keyword (str "h" (min 6 (count (trim hashes))))) (trim header) tail]))))

(defn ^:dynamic headers [lines]
  "Markdown supports two styles of headers, Setext and atx."
  (when-let [[header-level header-text tail]
             (some (fn [header-matcher] (header-matcher lines)) [atx-header settext-header])]
    [[header-level header-text] (vec tail)]))

(defn ^:dynamic hr-line [string]
  (some
    (fn [character]
      (and (only-contains (remove-whitespaces string) character) (>= (get (frequencies string) character) 3)))
    [\- \_ \*]))

(defn ^:dynamic horizontal-rule [lines]
  "You can produce a horizontal rule tag (<hr />) by placing
  three or more hyphens, asterisks, or underscores on a line
  by themselves. If you wish, you may use spaces between
  the hyphens or asterisks."
  (when (and (coll? lines) (hr-line (first lines)))
    [:hr (rest lines)]))

(defn ^:dynamic blank-line [lines]
  (when (and (coll? lines) (blank? (first lines)))
    [:blankline (rest lines)]))

(def matchers [blank-line
               horizontal-rule
               headers
               list-block
               paragraph])

(defn ^:dynamic parse [lines]
  (loop [parsed []
         remaining lines]
    (if (empty? remaining)
      parsed
      (if-let [[result unparsed]
               (some (fn [matcher] (matcher remaining)) matchers)]
        (recur (conj parsed result) unparsed)
        parsed))))