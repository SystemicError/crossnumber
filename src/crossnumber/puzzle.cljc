(ns crossnumber.puzzle
  (:require [clojure.core :as core]
            [clojure.string :as str]))

(defn factor
  "Returns a sorted vector of factors of n."
  ([n] (factor n [] 2))
  ([n factors candidate]
   (if (< n candidate)
     factors
     (if (= (mod n candidate) 0)
       (recur (/ n candidate) (conj factors candidate) candidate)
       (recur n factors (+ 1 candidate)))))
  )

(defn primes
  "(inefficiently) Gives all the primes less than n."
  ([n] (if (>= 2 n)
         (list)
         (if (>= 3 n)
           (list 2)
           (primes n (list 2 3) 5))))
  ([n ps candidate]
    (let [composite? (not (empty? (filter
                                    #(= 0 (mod candidate %))
                                    ps)))]
      (if (>= candidate n)
        ps
        (if composite?
          (recur n ps (+ 2 candidate))
          (recur n (conj ps candidate) (+ 2 candidate))))))
  )

(defn choose-digit
  "Chooses a random digit, excluding zero if given the 0th row or column."
  [r c]
  (if (or (= r 0) (= c 0))
    (+ (rand-int 9) 1)
    (rand-int 10)))

(defn choose-digits
  "Returns a randomized h by w matrix with no leading zeroes in any col or row."
  [w h]
  (apply vector (for [r (range h)]
                  (apply vector (for [c (range w)]
                                  (choose-digit r c))))))

(defn row-to-int
  "Assembles a row of single-digit ints into a number, most-significant digit first."
  ([row] (row-to-int row 0 1))
  ([row n place]
    (if (empty? row)
      n
      (recur (butlast row) (+ (* (last row) place) n) (* 10 place))))
  )

(defn col-to-int
  "Assembles a column into a number."
  [digits c]
  (row-to-int (map #(nth % c) digits))
  )

(defn alphabet
  "Returns an alphabet of n letters."
  [n]
  (for [x (range n)]
    (char (+ 65 x)))
  )

(defn factors-to-expression
  "Takes sorted factors and dictionary and turns them into nice algebraic expression."
  ([factors dict] (factors-to-expression factors dict []))
  ([factors dict expr]
    (let [f (first factors)
          n (count (filter #(= f %) factors))
          letter (dict f)]
      (if (empty? factors)
        (apply str (sort expr))
        (recur (filter #(not= f %) factors)
               dict
               (conj expr (str "(" letter (if (> n 1) (str "^" n)) ")"))))))
  )

(defn clue-to-hash-map
  "Takes a clue of the form (A)(B^3)(D^2) and turns it into a hash-map of letters to powers."
  [clue]
  (let [filtered (str/replace clue #"[^A-Z0-9]" "")
        terms (re-seq #"[A-Z][0-9]*" filtered)
        vars (for [t terms]  (- #?(:clj (int (nth t 0))
                                   :cljs (.charCodeAt t 0)) 65))
        exponents (for [t terms] (if (empty? (subs t 1))
                                   1
                                   #?(:clj (read-string (subs t 1))
                                      :cljs (js/parseInt (subs t 1)))))]
    (apply hash-map (interleave vars exponents)))
  )

(defn monomial
  "Gives a monomial fn based on a string input and a num of letters."
  [hm n]
  (fn [args] (apply * (for [x (range n)] (if (hm x)
                                             (Math/pow (nth args x) (hm x))
                                             1))))
  )

(defn cartesian-product
  "Take the cartesian product of several collections."
  ([spaces] (cartesian-product spaces (list (list))))
  ([spaces coords]
    (if (empty? spaces)
      coords
      (recur (butlast spaces)
             (apply concat (for [pt coords] (for [x (last spaces)] (cons x pt)))))
      ))
  )

(defn clue-to-letter-max
  "Estimates max possible value of a letter based on a single clue."
  [letter clue length]
  (let [ceiling (Math/pow 10 length)]
    (if (not (clue letter))
      ceiling
      (let [other-exp (- (apply + (vals clue)) (clue letter))
            self-exp (clue letter)]
        (Math/pow (/ ceiling (Math/pow 2 other-exp)) (/ 1.0 self-exp)))))
  )


(defn clues-to-letter-max
  "Estimates max possible value of a letter (specified by an int), given some hash-map representations of clues and the length of the rows/cols in that direction."
  [letter clues length]
  (apply min (for [clue clues] (clue-to-letter-max letter clue length)))
  )
  
(defn int-to-digits
  "Converts a number into a sequence of digits."
  ([n] (int-to-digits n (list)))
  ([n s]
   (if (= n 0)
     s
     (recur (int (/ n 10)) (cons (int (mod n 10)) s))))
  )

(defn transpose
  "Returns the transpose of a matrix."
  ([m]
   (for [r (range (count m))]
     (for [c (range (count (first m)))]
       (nth (nth m c) r))))
  )

(defn solves?
  "Checks if given tuple solves given row and column functions."
  [row-fns col-fns candidate]
  (let [rows (for [f row-fns] (f candidate))
        cols (for [f col-fns] (f candidate))
        num-rows (count row-fns)
        num-cols (count col-fns)
        row-min (Math/pow 10 (dec num-cols))
        col-min (Math/pow 10 (dec num-rows))
        row-max (Math/pow 10 (inc num-cols))
        col-max (Math/pow 10 (inc num-rows))
        row-digits (for [row rows] (int-to-digits row))
        col-digits (for [col cols] (int-to-digits col))]
    (and (>= (apply min rows) row-min)
         (>= (apply min cols) col-min)
         (< (apply max rows) row-max)
         (< (apply max cols) col-max)
         (= row-digits (transpose col-digits))))
  )

(defn clues-to-alphabet
  "Lists all letters present in clues."
  [clues]
  (let [all-clues (str/join clues)]
    (sort (core/set (filter #(not (str/blank? %)) (str/split all-clues #"[()^\d]")))))
  )

(defn crossnumber-solutions
  "Finds all possible solutions to a given crossnumber puzzle."
  ; clues are given as strings of the form "(A^2)(B)(C^4)"
  [row-clues col-clues]
  (let [num-rows (count row-clues)
        num-cols (count col-clues)
        letters (clues-to-alphabet (concat row-clues col-clues))
        num-letters (count letters)
        row-hms (for [clue row-clues] (clue-to-hash-map clue))
        col-hms (for [clue col-clues] (clue-to-hash-map clue))
        row-fns (for [hm row-hms] (monomial hm num-letters)) 
        col-fns (for [hm col-hms] (monomial hm num-letters))
        row-max (Math/pow 10 num-cols)
        col-max (Math/pow 10 num-rows)
        row-min (Math/pow 10 (dec num-cols))
        col-min (Math/pow 10 (dec num-cols))
        letter-maxes (for [n (range num-letters)] (min (clues-to-letter-max n row-hms num-cols)
                                                       (clues-to-letter-max n col-hms num-rows)))
	    letter-candidates (for [n letter-maxes] (primes n))
	    solution-candidates (cartesian-product letter-candidates)]
  ; from the cartesian product of the candidates, filter out everything that fails to meet these rules":
  ; the first digit of each row/col is not zero
  ; the number of digits in each row entry = num-cols
  ; the number of digits in each col entry = num-rows
  ; the jth digit of the ith row entry = the ith digit of the jth col entry
  ; then return what remains
  (filter #(solves? row-fns col-fns %) solution-candidates))
  )

