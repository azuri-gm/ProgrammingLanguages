;;; ITESM CEM, October 7, 2015.
;;; Clojure Source File
;;; Activity: Using the Sequence API
;;; Authors: A01165988 Eduardo Azuri Gaytán Martínez
;;;          A01165829 Eduardo Rodríguez Ruiz

(ns higherorder
  (:use clojure.test))

(defn positives
  "takes a list of numbers lst as its argument, and returns a new list that
  only contains the positive numbers of lst. "
  [lst]
  (filter pos? lst))

(defn dot-product
  "Takes two arguments: the lists a and b. It returns the result of performing
  the dot product of a times b."
  [lst lst2]
  (reduce +(map * lst lst2)))

(defn insert
  "takes two arguments: a number n and a list of numbers lst in ascending order. It returns a new list with
  the same elements as lst but inserting n in its corresponding place."
  [n lst]
  (let [[less greater] (split-with #(< % n) lst)]
    (concat less (list n) greater)))

(defn replic
  "takes two arguments: a list lst and an integer number n, where n ≥ 0. It returns a new list
  that replicates n times each element contained in lst."
  [n lst]
  (mapcat #(repeat n %) lst))

(defn expand
  "takes a list lst as its argument. It returns a list where the first element of lst appears one time,
  the second elements appears two times, the third element appears three times, and so on."
  [lst]
  (apply concat(map-indexed(fn [index,n] (repeat (inc index) n))lst)))

(defn largest
  " takes as argument a nonempty list of numbers lst. It returns the largest value contained in lst. Use the reduce function to solve this problem.
  Do not use the predefined max or min functions."
  [lst]
  (reduce (fn [p s] (if (>= p s) p s)) lst))

(defn drop-every
  "takes two arguments: an integer number n, where n ≥ 1, and a list lst. It returns a new list
  that drops every n-th element from lst."
  [n lst]
  (apply concat (partition (- n 1) n lst)))

(defn rotate-left
  "takes two arguments: an integer number n and a list lst. It returns the list that results from rotating lst a total of n elements to the left.
  If n is negative, it rotates to the right."
  [n lst]
  (apply concat(reverse (split-at (mod n (count lst)) lst))))

(defn join
  "takes two arguments: any object sep and a list lst.
  It returns a string created by converting each element of lst to a string, separated by sep."
  [obj lst]
  (apply str(interpose  (str obj) lst )))

(defn pi
  "Takes an integer argument n and returns an approximation of π using the
   following series composed of n terms: π = 4(1 − 1/3 + 1/5 − 1/7 + 1/9 − 1/11 + ...)"
  [n]
  (->>
   (iterate #(+ 2 %) 1)
   (map #(/ 1.0 %) ,)
   (map-indexed (fn [index x] (if (even? (inc index)) (* -1 x) x)) ,)
   (take n ,)
   (reduce + ,)
   (* 4 ,)))



;;;--------------------------------------------------
;;;                      TESTS
;;;--------------------------------------------------

(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))

(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4) '(-4.5 3.0 1.5 0.9 0.0)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 5 5 6 7 9 16) (insert 5 '(1 3 5 5 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) (replic 4 '(1 2 3 4)))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e) (expand '(a b c d e)))))

(deftest test-largest
  (is (= 31 (largest '(31))))
  (is (= 5 (largest '(1 2 3 4 5))))
  (is (= -1 (largest '(-1 -2 -3 -4 -5))))
  (is (= 52 (largest '(32 -1 45 12 -42 52 17 0 21 2)))))

(deftest test-drop-every
  (is (= () (drop-every 5 ())))
  (is (= '(1 2 3) (drop-every 4 '(1 2 3 4))))
  (is (= '(1 3 5 7) (drop-every 2 '(1 2 3 4 5 6 7 8))))
  (is (= '(1 3 5 7 9) (drop-every 2 '(1 2 3 4 5 6 7 8 9))))
  (is (= '(a b d e g h j) (drop-every 3 '(a b c d e f g h i j))))
  (is (= '(a b c d e f g h i j)
         (drop-every 20 '(a b c d e f g h i j))))
  (is (= () (drop-every 1 '(a b c d e f g h i j)))))

(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

(deftest test-join
  (is (= "" (join \$ ())))
  (is (= "a" (join "..." '(a))))
  (is (= "alphabetagamma" (join nil '("alpha" "beta" "gamma"))))
  (is (= "alpha$beta$gamma" (join \$ '("alpha" "beta" "gamma"))))
  (is (= "1 and 2 and 3 and 4" (join " and " '(1 2 3 4))))
  (is (= "a,b,c,d,e,f" (join \, '(a b c d e f)))))

(deftest test-pi
  (is (= 4.0000000000000000 (pi 1)))
  (is (= 2.6666666666666670 (pi 2)))
  (is (= 3.0418396189294032 (pi 10)))
  (is (= 3.1405926538397940 (pi 1000)))
  (is (= 3.1414926535900345 (pi 10000)))
  (is (= 3.1415826535897198 (pi 100000)))
  (is (= 3.1415916535897743 (pi 1000000))))

(run-tests)
