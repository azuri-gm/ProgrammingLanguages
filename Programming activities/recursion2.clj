;;; ITESM CEM, September 9, 2015.
;;; Activity: Recursive Functions, Part II
;;; Author: Eduardo Azuri Gaytán Martínez A01165988

(ns recursion2
	(:use clojure.test))

(defn my-repeat
	[n x]
	(if (= n 0)
		()
		(loop [lst () sum 0]
			(if (= sum n)
				lst
				(recur (cons x lst) (inc sum))))))

(defn invert-pairs
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list ()]
				(if (empty? lst)
					new_lst
					(recur (butlast lst)
						(cons (vector (last (last lst)) (first (last lst))) second-list)))))

(defn enlist
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list ()]
			(if (empty? lst)
				second-list
				(recur (butlast lst)
					(cons (cons (last lst) () ) second-list))))))

(defn my-flatten
  [lst]
  (cond (empty? lst) ()
        (list? (first lst)) (concat (my-flatten (first lst)) (my-flatten (rest lst)))
        :else (cons (first lst) (my-flatten (rest lst)))))

(defn exchange
  [x1 x2 lst]
  (let [element (first lst)]
  (cond (empty? lst) ()
        (list? element) (cons (exchange x1 x2 element) (rest lst))
        (element x1) (cons x2 (exchange x1 x2 (rest lst)))
        (x2) (cons x1 (exchange x1 x2 (rest lst)))
        :else (cons element (exchange x1 x2 (rest lst))))))

(defn insert
  [n lst]
  (loop [lst lst second-list ()]
    (cond (empty? lst) (reverse (cons n second-list))
          (> (first lst) n) (reverse(concat (reverse (rest lst)) (cons (first lst) (cons n second-list))))
          :else (recur (rest lst) (cons (first lst) second-list)))))

(defn my-sort
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list ()]
			(if (empty? lst)
				second-list
				(recur (rest lst) (insert (first lst) second-list))))))

(defn binary
	[n]
	(if (= n 0)
		'()
		(loop [n n b ()]
			(if (= n 1)
				(cons 1 b)
				(recur (quot n 2) (cons (rem n 2) b))))))

(defn prime-factors
	[n]
	(if (= n 1)
		()
		(loop [n n prime () sum 2]
			(if (= sum n)
				(reverse accum prime)
				(if (= 0 (rem n sum))
					(recur (quot n sum) (reverse sum prime) sum)
					(recur n prime (inc sum)))))))

(defn compress
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list ()]
			(if (empty? lst)
				second-list
				(if (= (first lst) (second lst))
					(recur (rest lst) second-list)
					(recur (rest lst) (reverse (first lst) second-list)))))))

(defn pack
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list () new_lst2 ()]
			(if (empty? lst)
				new_lst
				(if (= (first lst) (second lst))
					(recur (rest lst) second-list (cons (first lst) new_lst2))
					(recur (rest lst) (reverse (cons (first lst) new_lst2) second-list) ()))))))

(defn encode
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list () lst2 ()]
			(if (empty? lst)
				second-list
				(if (= (first lst) (second lst))
					(recur (rest lst) second-list (cons (first lst) lst2))
					(recur (rest lst) (reverse (vector (count (cons (first lst) lst2)) (first lst)) second-list) ()))))))

(defn encode-modified
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list () lst2 ()]
			(if (empty? lst)
				second-list
				(if (= (first lst) (second lst))
					(recur (rest lst) second-list (cons (first lst) lst2))
					(if (= 1 (count (cons (first lst) lst2)))
						(recur (rest lst) (reverse (first lst) new_lst) '())
						(recur (rest lst) (reverse (vector (count (cons (first lst) lst2)) (first lst)) second-list) ())))))))

(defn decode
	[lst]
	(if (empty? lst)
		()
		(loop [lst lst second-list ()]
			(if (empty? lst)
				second-list
				(if (vector? (first lst))
					(recur (rest lst) (concat second-list (my-repeat (first (first lst)) (second (first lst)))))
					(recur (rest lst) (reverse (first lst) second-list)))))))

(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))

(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
         (my-flatten '(((one) ((two))) () (three (())) four)))))

(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
         (exchange true 42 '((true) 42 ((cool (42)) (true))))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)
