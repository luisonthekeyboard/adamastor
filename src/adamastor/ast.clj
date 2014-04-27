(ns adamastor.ast
  (:use [adamastor.utils]))

(def line-not-preceeded-by-hash #"^(?!#).*$")
(def line-only-with-dashes #"^-+$")
(def line-only-with-equals #"^=+$")

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


(defn ^:dynamic headers [lines]
  "Markdown supports two styles of headers, Setext and atx.
  Atx-style headers use 1-6 hash characters at the start of the line,
  corresponding to header levels 1-6. Optionally, you may
  “close” atx-style headers. This is purely cosmetic — you
  can use this if you think it looks better. The closing
  hashes don’t even need to match the number of hashes used
  to open the header."
  (when-let [[header-level header-text tail]
             (some (fn [header-matcher] (header-matcher lines)) '(atx-header settext-header))]
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
      [[:rh ] tail]
      false)))
