(ns fop-tax.xlsx
  "Minimal .xlsx reader: an .xlsx is a zip of XML parts, so java.util.zip plus
  clojure.data.xml (both built into Babashka) are enough. Reads the first sheet
  only, which is all PrivatBank statements contain."
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str])
  (:import [java.util.zip ZipFile]))

(defn- parse-entry [^ZipFile z entry-name]
  (when-let [e (.getEntry z entry-name)]
    (xml/parse (.getInputStream z e) :skip-whitespace true)))

(defn- tag? [t node]
  (and (map? node) (= t (name (:tag node)))))

(defn- children [t node]
  (filter (partial tag? t) (:content node)))

(defn- text
  "All text under a node, concatenated (handles rich-text runs in shared strings)."
  [node]
  (apply str (map #(if (string? %) % (text %)) (:content node))))

(defn- column [cell-ref]
  (str/replace cell-ref #"\d+" ""))

(defn- cell-value [shared-strings cell]
  (let [t (get-in cell [:attrs :t])
        v (some-> (first (children "v" cell)) text)]
    (case t
      "s"         (nth shared-strings (parse-long v))
      "inlineStr" (some-> (first (children "is" cell)) text)
      v)))

(defn read-rows
  "Returns the first sheet as a vector of rows; each row is a map of column
  letter (\"A\", \"B\", ...) to its raw string value. Numbers stay strings so the
  caller can parse them into BigDecimal without float rounding."
  [path]
  (with-open [z (ZipFile. (str path))]
    (let [shared (some->> (parse-entry z "xl/sharedStrings.xml")
                          (children "si")
                          (mapv text))
          sheet  (parse-entry z "xl/worksheets/sheet1.xml")
          data   (first (children "sheetData" sheet))]
      (vec (for [row (children "row" data)]
             (into {}
                   (for [c (children "c" row)
                         :let [v (cell-value shared c)]
                         :when (some? v)]
                     [(column (get-in c [:attrs :r])) v])))))))
