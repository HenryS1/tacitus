(defpackage tacitus/tests/tacitus
  (:use :cl
        :tacitus
        :rove))
(in-package :tacitus/tests/tacitus)

(deftest to-array
  (testing "should produce numbers between start and end"
    (ok (equalp (to-array (between 5 10))
                #(5 6 7 8 9 10))))
  (testing "should produce the results of applying a transformation"
    (ok (equalp (to-array (fmap (lambda (x) (* x 5)) (between 5 10)))
                #(25 30 35 40 45 50)))))

(deftest append-ranges 
  (testing "should concatenate ranges"
    (ok (equalp (to-array (append-ranges (between 0 5) (between 6 10)))
                #(0 1 2 3 4 5 6 7 8 9 10))))
  (testing "preserve transformations on each range that is appended"
    (ok (equalp (to-array (append-ranges (fmap (lambda (x) (* x -2)) (between 0 5))
                                         (fmap (lambda (x) (- x 3)) (between 6 10))))
                #(0 -2 -4 -6 -8 -10 3 4 5 6 7)))))

(deftest index
  (testing "should retrieve a single element from a range"
    (ok (= (index (between 0 10) 2) 2)))
  (testing "return a transformed element from a transformed range"
    (ok (= (index (fmap (lambda (x) (* x 3)) (between 0 10)) 2) 6)))
  (testing "should return nil for an index outside the range"
    (ok (null (index (between 4 6) 3))))
  (testing "not return nil for elements within the range"
    (ok (= (index (between 4 6) 0) 4))
    (ok (= (index (between 4 6) 2) 6))))

(deftest fmap-rec
  (testing "should recurse on previous elements in a range"
    (ok (equalp (to-array (fmap-rec (lambda (i) (if (= i 0) 0 (+ i (index self (- i 1))))) 
                                    (between 0 10)))
                #(0 1 3 6 10 15 21 28 36 45 55)))))

(deftest memoization
  (testing "should prevent recomputation"
    (ok (= (index (fmap-rec (lambda (i) (if (or (= i 0) (= i 1)) 1
                                            (+ (index self (- i 1)) (index self (- i 2)))))
                            (between 0 2000 :memoize t)) 200)
           453973694165307953197296969697410619233826))))

(deftest flatmap
  (testing "should flatten nested ranges"
    (ok (equalp (to-array (flatmap (lambda (i) (between 0 i)) (between 0 4)))
                #(0 0 1 0 1 2 0 1 2 3 0 1 2 3 4)))))

(deftest reduce-range 
  (testing "should combine elements in range with a binary operation"
    (ok (= (reduce-range #'+ (between 0 10)) 55)))
  (testing "should combine elements in a transformed range"
    (ok (= (reduce-range #'+ (fmap (lambda (x) (* x 2)) (between 4 6))) 30))))

(deftest zip
  (testing "should provide pairs of elements from each range"
    (ok (equalp (to-array (zip (between 0 4) (between 5 9)))
                #((0 . 5) (1 . 6) (2 . 7) (3 . 8) (4 . 9)))))
  (testing "should have size the minimum of the two ranges sizes"
    (ok (= (range-size (zip (between 0 4) (between 0 100))) 5))))
