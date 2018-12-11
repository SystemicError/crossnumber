(ns crossnumber.puzzle-test
  (:require [clojure.test :refer :all]
            [crossnumber.puzzle :refer :all]
            [clojure.core :as core]
            [clojure.string :as str]))

(deftest factor-test
  (testing "Factor fail."
    (is (= (core/set (factor 30)) (core/set (list 2 3 5))))
    ))

(deftest primes-test
  (testing "Primes fail."
    (is (= (sort (primes 2)) (list)))
    (is (= (sort (primes 3)) (list 2)))
    (is (= (sort (primes 4)) (list 2 3)))
    (is (= (sort (primes 6)) (list 2 3 5)))
    ))

(deftest choose-digit-test
  (testing "Choose digit fail."
    (is (not= 0 (min (for [n (range 100)] (choose-digit 1 0)))))
    (is (not= 0 (min (for [n (range 100)] (choose-digit 0 1)))))
    ))

(deftest choose-digits-test
  (testing "Choose digits fail."
    (let [digits (choose-digits 2 3)]
      (is (= (count digits) 3))
      (is (= (count (first digits)) 2))
      (is (not= 0 (min (apply min (first digits)) (first (nth digits 1)) (first (nth digits 2)))))
      )
    ))

(deftest row-to-int-test
  (testing "Row to int fail."
    (is (= 432 (row-to-int (list 4 3 2))))
    ))

(deftest col-to-int-test
  (testing "Col to int fail."
    (is (= 432 (col-to-int (list (list 1 4 2)
                                 (list 2 3 2)
                                 (list 2 2 2))
                           1)))
    ))

(deftest alphabet-test
  (testing "Alphabet fail."
    (is (= (list \A \B \C \D \E) (alphabet 5)))
    ))

(deftest factors-to-expression-test
  (testing "Factors to expression fail."
    (is (= "(A)(B^2)(D^3)" (factors-to-expression (list 2 3 3 7 7 7)
                                                  {2 "A" 3 "B" 5 "C" 7 "D"})))
    ))

(deftest clue-to-hash-map-test
  (testing "Clue to hash map fail."
    (is (= {0 1 1 2 3 3} (clue-to-hash-map "(A)(B^2)(D^3)")))
    ))

(deftest monomial-test
  (testing "Monomial fail."
    (let [f (monomial {0 0 1 2 2 0 3 1} 4)]
      (is (= ((fn [w x y z] (* (* x x) z)) 1.0 2.0 3.0 4.0)
             (f [1.0 2.0 3.0 4.0]))))
    ))

(deftest cartesian-product-test
  (testing "Cartesian product fail."
    (is (= (list (list 1) (list 2) (list 3))
           (cartesian-product (list (list 1 2 3)))))
    (is (= (core/set (list (list 1 :a) (list 1 :b) (list 2 :a) (list 2 :b)))
           (core/set (cartesian-product (list (list 1 2) (list :a :b))))))
    ))

(deftest clue-to-letter-max-test
  (testing "Clue to letter max fail."
    (is (= 100.0
           (clue-to-letter-max 1 (clue-to-hash-map "(A)") 2)))
    (is (= 50.0
           (clue-to-letter-max 1 (clue-to-hash-map "(A)(B)") 2)))
    (is (= (Math/pow 50 0.5)
           (clue-to-letter-max 1 (clue-to-hash-map "(A)(B^2)") 2)))
    (is (= 25.0
           (clue-to-letter-max 1 (clue-to-hash-map "(A^2)(B)") 2)))
    (is (= 5.0
           (clue-to-letter-max 1 (clue-to-hash-map "(A^2)(B^2)") 2)))
    (is (= 5.0
           (clue-to-letter-max 1 (clue-to-hash-map "(A)(B^2)(C)") 2)))
    ))


(deftest clues-to-letter-max-test
  (testing "Clues to letter max fail."
    (is (= 10.0
           (clues-to-letter-max 2
                                (list (clue-to-hash-map "(A)(B)(C)")
                                      (clue-to-hash-map "(B^2)")
                                      (clue-to-hash-map "(C^2)"))
                                2)))
    ))

(deftest int-to-digits-test
  (testing "Int to digits fail."
    (is (= (list 1 2 3) (int-to-digits 123)))
    ))

(deftest transpose-test
  (testing "Transpose fail."
    (is (= (transpose (list (list 1 2 3)
                            (list 4 5 6)
                            (list 6 7 8)))
           (list (list 1 4 6)
                 (list 2 5 7)
                 (list 3 6 8))))
    ))

(deftest solves?-test
  (testing "Solves? fail."
    (let [row-clue0 (clue-to-hash-map "(B^2)(C)")
          row-clue1 (clue-to-hash-map "(B)(D)")
          col-clue0 (clue-to-hash-map "(A)")
          col-clue1 (clue-to-hash-map "(B^3)(C)")]
      (is (true? (solves? (list (monomial row-clue0 4)
                                (monomial row-clue1 4))
                          (list (monomial col-clue0 4)
                                (monomial col-clue1 4))
                          (list 13 2 3 17))))
      (is (false? (solves? (list (monomial row-clue0 4)
                                (monomial row-clue1 4))
                          (list (monomial col-clue0 4)
                                (monomial col-clue1 4))
                          (list 13 5 3 17)))))
    ))

(deftest clues-to-alphabet-test
  (testing "Clues to alphabet fail."
    (let [clues (list "(B^2)(C)" "(B)(D)" "(A)" "(B^3)(C)")]
      (is (= (list "A" "B" "C" "D")
             (clues-to-alphabet clues))))
    ))

(deftest crossnumber-solutions-test
  (testing "Crossnumber solutions fail."
    (let [row-clues (list "(B^2)(C)" "(B)(D)")
          col-clues (list "(A)" "(B^3)(C)")]
      (is (= (core/set (list (list 19 2 3 47)
                             (list 17 2 3 37)
                             (list 13 2 3 17)
                             (list 11 2 3 7)))
             (core/set (crossnumber-solutions row-clues col-clues)))))
    ))
