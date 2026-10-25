(ns fop-tax.report
  "Quarterly single-tax (5%) and military-levy report for a group 3 FOP,
  built from PrivatBank statement exports. Totals are cumulative from the start
  of the year, like the declaration (and Taxer) shows them."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [fop-tax.statement :as st])
  (:import [java.math RoundingMode]
           [java.time LocalDate]))

;; Rates by year. Military levy for group 3 is 1% of income since 01.01.2025.
(def rates
  {2025 {:single-tax 0.05M :military-levy 0.01M}
   2026 {:single-tax 0.05M :military-levy 0.01M}})

;; --- money -----------------------------------------------------------------

(defn round2 [^BigDecimal x] (.setScale x 2 RoundingMode/HALF_UP))

(defn fmt
  "Taxer style: no grouping, comma decimal separator."
  [x]
  (str/replace (str (round2 (bigdec x))) "." ","))

(defn- sum [xs] (reduce + 0M (remove nil? xs)))

;; --- locating statements ---------------------------------------------------

(def ^:private quarter-dir-re #"^(\d{4})_0?([1-4])(?:\D.*)?$")

(defn- resolve-quarter-dir [arg]
  (let [candidates [(fs/path arg) (fs/path ".." arg)]]
    (or (some #(when (fs/directory? %) (fs/absolutize %)) candidates)
        (throw (ex-info (str "Statement folder not found: " arg) {})))))

(defn statement-files
  "All .xlsx files in sibling folders of the same year with quarter <= target."
  [quarter-dir]
  (let [[_ y q] (re-matches quarter-dir-re (str (fs/file-name quarter-dir)))
        _       (when-not y
                  (throw (ex-info (str "Folder name must look like 2026_3: " quarter-dir) {})))
        year    (parse-long y)
        quarter (parse-long q)
        dirs    (->> (fs/list-dir (fs/parent quarter-dir))
                     (filter fs/directory?)
                     (filter #(when-let [[_ y2 q2] (re-matches quarter-dir-re (str (fs/file-name %)))]
                                (and (= year (parse-long y2)) (<= (parse-long q2) quarter))))
                     (sort))]
    {:year    year
     :quarter quarter
     :dirs    dirs
     :files   (->> dirs (mapcat #(fs/glob % "*.xlsx")) (map str) sort)}))

(defn quarter-of [^LocalDate d] (inc (quot (dec (.getMonthValue d)) 3)))

(defn- quarter-bounds [year q]
  (let [start (LocalDate/of (int year) (int (inc (* 3 (dec q)))) 1)]
    [start (.minusDays (.plusMonths start 3) 1)]))

;; --- checks ----------------------------------------------------------------

(defn- reconcile
  "Each statement states its credit total and count in the header; compare them
  with the rows we parsed, so a parsing problem cannot silently drop income."
  [{:keys [file credit-total credit-count transactions]}]
  (let [credits (filter #(pos? (:amount %)) transactions)
        total   (sum (map :amount credits))]
    (cond-> []
      (and credit-total (not= 0 (compare total credit-total)))
      (conj (str "Сума кредиту не збігається з шапкою виписки: " (fmt total)
                 " ≠ " (fmt credit-total) " (" (fs/file-name file) ")"))
      (and credit-count (not= credit-count (count credits)))
      (conj (str "Кількість кредитових операцій не збігається: " (count credits)
                 " ≠ " credit-count " (" (fs/file-name file) ")")))))

(defn- coverage-warnings
  "Every quarter up to the target must be covered by at least one statement;
  otherwise the cumulative total is incomplete."
  [statements year quarter]
  (let [accounts (sort (distinct (map :iban statements)))]
    (for [q (range 1 (inc quarter))
          :let [[qs qe] (quarter-bounds year q)
                covers? (fn [s] (and (:period-from s) (:period-to s)
                                     (not (.isAfter ^LocalDate (:period-from s) qs))
                                     (not (.isBefore ^LocalDate (:period-to s) qe))))
                covered (set (map :iban (filter covers? statements)))]
          acc accounts
          :when (not (covered acc))]
      (str q " кв: немає виписки за весь квартал по рахунку " acc
           " (" (some #(when (= acc (:iban %)) (:currency %)) statements) ")"
           (when (empty? covered) " — ЖОДНОЇ виписки за квартал")))))

;; --- report ----------------------------------------------------------------

(defn build [quarter-dir-arg]
  (let [{:keys [year quarter files]} (statement-files (resolve-quarter-dir quarter-dir-arg))
        _          (when (empty? files) (throw (ex-info "No .xlsx statements found" {})))
        statements (mapv st/read-statement files)
        credits    (->> statements
                        (mapcat :transactions)
                        (filter #(pos? (:amount %)))
                        ;; overlapping exports must not double-count
                        (reduce (fn [m t] (assoc m [(:account t) (:reference t) (:date t) (:amount t)] t)) {})
                        vals
                        (filter #(and (= year (.getYear ^LocalDate (:date %)))
                                      (<= (quarter-of (:date %)) quarter)))
                        (map #(merge % (st/classify %) {:quarter (quarter-of (:date %))}))
                        (sort-by (juxt :date :time)))
        income     (filter #(= :income (:kind %)) credits)
        by-q       (into (sorted-map)
                         (for [q (range 1 (inc quarter))]
                           [q (sum (map :uah (filter #(= q (:quarter %)) income)))]))
        cum        (fn [upto] (sum (for [[q v] by-q :when (<= q upto)] v)))
        rate       (get rates year)
        tax        (fn [k amount] (round2 (* amount (get rate k))))
        total      (cum quarter)
        prev       (cum (dec quarter))]
    {:year year :quarter quarter :files files
     :tax-ids (distinct (keep :tax-id statements))
     :by-quarter by-q
     :income-total total
     :single-tax {:total (tax :single-tax total)
                  :previous (tax :single-tax prev)
                  :due (- (tax :single-tax total) (tax :single-tax prev))}
     :military-levy {:total (tax :military-levy total)
                     :previous (tax :military-levy prev)
                     :due (- (tax :military-levy total) (tax :military-levy prev))}
     :credits credits
     :warnings (concat (when-not rate [(str "Немає ставок для " year " року в fop-tax.report/rates")])
                       (mapcat reconcile statements)
                       (coverage-warnings statements year quarter)
                       (for [c income :when (:flag? c)]
                         (str "Зараховано як дохід, перевірте вручну: " (:date c) " "
                              (fmt (or (:uah c) (:amount c))) " — " (:purpose c)))
                       (for [c income :when (nil? (:uah c))]
                         (str "Немає гривневого еквівалента: " (:date c) " " (:amount c)
                              " " (:currency c))))}))

;; --- printing --------------------------------------------------------------

(defn- line [label value]
  (println (format "  %-66s %14s грн" label (fmt value))))

(defn- short-name
  "Long legal form hides the actual name in a narrow column."
  [s]
  (str/replace (str s) #"(?iu)товариство\s+з\s+обмеженою\s+в[iі]дпов[iі]дальн[iі]стю" "ТОВ"))

(defn- trunc [s n] (let [s (str s)] (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

(defn- print-credits [title rows]
  (println)
  (println title)
  (if (empty? rows)
    (println "  —")
    (doseq [{:keys [date amount currency nbu-rate uah reason counterparty flag?]} rows]
      (println (format "  %s %s %14s  %-38s %s"
                       (if flag? "!" " ")
                       (str date)
                       (fmt (or uah amount))
                       (trunc (short-name counterparty) 38)
                       (str (when (not= "UAH" currency)
                              (str amount " " currency " × " nbu-rate "; "))
                            reason))))))

(defn print-report [{:keys [year quarter files tax-ids by-quarter income-total
                            single-tax military-levy credits warnings]}
                    {:keys [all?]}]
  (println (str "ФОП, РНОКПП " (str/join ", " tax-ids) " — " quarter " квартал " year
                " (наростаючим підсумком з 01.01." year ")"))
  (println (str "Виписки (" (count files) "):"))
  (doseq [f files] (println "  " (str (fs/relativize (fs/parent (fs/parent f)) f))))
  (println)
  (println "Дохід по кварталах:")
  (doseq [[q v] by-quarter] (println (format "  %d кв  %14s грн" q (fmt v))))
  (println (format "  Разом %14s грн" (fmt income-total)))
  (println)
  (println "Як у декларації (Taxer):")
  (line "Сума доходу за звітний період, що оподатковується за ставкою 5%" income-total)
  (line "Нараховано єдиного податку за попередній звітний період" (:previous single-tax))
  (line "Єдиний податок до сплати у звітному періоді" (:due single-tax))
  (line "Нараховано військового збору за попередній звітний період" (:previous military-levy))
  (line "Військовий збір до сплати у звітному періоді" (:due military-levy))
  (println)
  (line (str "Разом до сплати за " quarter " кв") (+ (:due single-tax) (:due military-levy)))
  (let [shown (if all? credits (filter #(= quarter (:quarter %)) credits))
        scope (if all? (str "1–" quarter " кв") (str quarter " кв"))]
    (print-credits (str "Враховано як дохід, " scope ":") (filter #(= :income (:kind %)) shown))
    (print-credits (str "Не враховано (надходження, що не є доходом), " scope ":")
                   (filter #(= :excluded (:kind %)) shown)))
  (println)
  (if (seq warnings)
    (do (println "УВАГА:") (doseq [w warnings] (println "  !" w)))
    (println "Перевірки: суми виписок збігаються, усі квартали покриті виписками.")))

(defn -main [& args]
  (let [all?  (some #{"--all" "-a"} args)
        [dir] (remove #(str/starts-with? % "-") args)]
    (if-not dir
      (do (println "Usage: bb report <YEAR_Q folder, e.g. 2026_3> [--all]")
          (System/exit 2))
      (print-report (build dir) {:all? all?}))))
