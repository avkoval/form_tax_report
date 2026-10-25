(ns fop-tax.statement-test
  (:require [clojure.test :refer [deftest is testing]]
            [fop-tax.report :as report]
            [fop-tax.statement :as st]))

(defn- uah [purpose counterparty amount]
  {:amount amount :purpose purpose :counterparty counterparty :account-currency "UAH"})

(deftest classify-uah-credits
  (testing "client payment is income"
    (let [r (st/classify (uah "Оплата за послуги за липень 2026 р. згiдно договору" "ТОВ \"Приклад\"" 10000M))]
      (is (= :income (:kind r)))
      (is (= 10000M (:uah r)))
      (is (not (:flag? r)))))
  (testing "currency sale proceeds are not income (Latin i as the bank writes it)"
    (is (= :excluded (:kind (st/classify (uah "Гривнi вiд продажу 100.00 USD по курсу 41.50"
                                              "Продаж UAH клiєнтiв" 4150M))))))
  (testing "own funds are not income"
    (is (= :excluded (:kind (st/classify (uah "ВЛАСНI КОШТИ" "ПЕТРЕНКО IВАН" 1000M)))))
    (is (= :excluded (:kind (st/classify (uah "Переказ власних коштiв." "Тр.рахунок" 1000M))))))
  (testing "unknown credit is counted and flagged"
    (let [r (st/classify (uah "Повернення помилково списаних коштів" "Банк" 10M))]
      (is (= :income (:kind r)))
      (is (:flag? r)))))

(deftest classify-currency-credit
  (let [r (st/classify {:amount 100M :currency "USD" :account-currency "USD"
                        :uah-equivalent 4123.45M
                        :purpose "Payment for the provision of IT services"})]
    (is (= :income (:kind r)))
    (is (= 4123.45M (:uah r)))
    (is (not (:flag? r)))))

(deftest parsing-helpers
  (is (= 123456.78M (st/parse-amount "123 456.78")))
  (is (= 41.5M (st/parse-amount "41,5000000")))
  (is (= "1234567,89" (report/fmt 1234567.89M)))
  (is (= 3 (report/quarter-of (st/parse-date "30.09.2026")))))
