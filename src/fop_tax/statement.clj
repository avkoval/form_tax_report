(ns fop-tax.statement
  "Parses a PrivatBank business statement (.xlsx) into transactions and
  classifies the incoming ones for the single tax."
  (:require [clojure.string :as str]
            [fop-tax.xlsx :as xlsx])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(def ^:private date-fmt (DateTimeFormatter/ofPattern "dd.MM.yyyy"))

(defn parse-date [s] (LocalDate/parse (str/trim s) date-fmt))

(defn parse-amount
  "Statement numbers look like \"46500.0\", \"913 996.94\" or \"43,9617000\"."
  [s]
  (when s
    (bigdec (-> s (str/replace #"[\s ]" "") (str/replace "," ".")))))

(defn normalize
  "PrivatBank mixes Latin i/I into Ukrainian words (\"Гривнi\", \"ВЛАСНI\").
  Lower-case and map them to Cyrillic і so regexes can be written in Ukrainian."
  [s]
  (-> (str s) str/lower-case (str/replace "i" "і")))

;; --- header ----------------------------------------------------------------

(defn- find-cell [rows re]
  (some (fn [row] (some #(re-find re %) (vals row))) rows))

(defn- header-info [rows path]
  (let [[_ iban cur] (or (find-cell rows #"рахунок № (\S+), валюта (\w+)")
                         (throw (ex-info "No account line in statement" {:file path})))
        [_ from to]  (find-cell rows #"період з\s*(\d{2}\.\d{2}\.\d{4}) по (\d{2}\.\d{2}\.\d{4})")
        [_ tax-id]   (find-cell rows #"РНОКПП (\d+)")
        [_ credit-total credit-count]
        (find-cell rows #"Разом за кредитом:\s*([-\d\s .,]+?)\s*\(Операцій: (\d+)\)")]
    {:file         (str path)
     :iban         iban
     :currency     cur
     :tax-id       tax-id
     :period-from  (some-> from parse-date)
     :period-to    (some-> to parse-date)
     :credit-total (parse-amount credit-total)
     :credit-count (some-> credit-count parse-long)}))

;; --- transactions ----------------------------------------------------------

(def ^:private columns
  {"Дата проводки"             :date
   "Час проводки"              :time
   "Сума"                      :amount
   "Валюта"                    :currency
   "Курс НБУ"                  :nbu-rate
   "Сума еквівалент у гривні"  :uah-equivalent
   "Призначення платежу"       :purpose
   "ЄДРПОУ"                    :counterparty-code
   "Назва контрагента"         :counterparty
   "Рахунок контрагента"       :counterparty-iban
   "Референс"                  :reference})

(defn- header-row-index [rows]
  (or (first (keep-indexed (fn [i row] (when (some #{"Дата проводки"} (vals row)) i)) rows))
      (throw (ex-info "No transaction table header (\"Дата проводки\")" {}))))

(defn- ->transaction [col->key row]
  (let [m (into {} (for [[col v] row :let [k (col->key col)] :when k] [k v]))]
    (-> m
        (update :date parse-date)
        (update :amount parse-amount)
        (update :nbu-rate parse-amount)
        (update :uah-equivalent parse-amount))))

(defn read-statement [path]
  (let [rows     (xlsx/read-rows path)
        info     (header-info rows path)
        hdr-i    (header-row-index rows)
        col->key (into {} (for [[col title] (nth rows hdr-i)
                                :let [k (columns (str/trim title))] :when k]
                            [col k]))
        date-col (some (fn [[col k]] (when (= k :date) col)) col->key)]
    (assoc info :transactions
           (->> (subvec rows (inc hdr-i))
                (filter #(seq (get % date-col)))
                (mapv #(assoc (->transaction col->key %)
                              :account (:iban info)
                              :account-currency (:currency info)))))))

;; --- classification --------------------------------------------------------

(defn classify
  "Classifies an incoming (positive) transaction. Returns
  {:kind :income|:excluded, :reason \"...\", :uah amount, :flag? bool}.

  Rules (single tax group 3, ст. 292 ПКУ):
  - proceeds of selling currency are not income: the currency was already
    counted at the NBU rate on the day it arrived;
  - transfers of the entrepreneur's own funds are not income;
  - a foreign-currency credit is income at the NBU rate on the day of receipt
    (the bank's \"Сума еквівалент у гривні\" column);
  - everything else is income. A credit that does not look like a payment for
    services is still counted but flagged for manual review."
  [{:keys [amount purpose counterparty account-currency uah-equivalent]}]
  (let [p (normalize purpose)
        c (normalize counterparty)]
    (cond
      (or (re-find #"^гривн[іi] в[іi]д продажу" p)
          (re-find #"продаж uah кл[іi]єнт" c))
      {:kind :excluded :reason "продаж валюти (дохід врахований при зарахуванні валюти)"}

      (re-find #"власн(их|і) кошт" p)
      {:kind :excluded :reason "переказ власних коштів"}

      (not= "UAH" account-currency)
      (if uah-equivalent
        {:kind :income :uah uah-equivalent
         :reason "валюта за курсом НБУ на дату зарахування"
         :flag? (not (re-find #"payment|оплат|послуг|serv" p))}
        {:kind :income :uah nil :flag? true
         :reason "валютне зарахування без гривневого еквівалента у виписці"})

      :else
      {:kind :income :uah amount
       :reason "оплата від контрагента"
       :flag? (not (re-find #"оплат|послуг|payment|serv|рахун|догов" p))})))
