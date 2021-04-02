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
                #(0 1 2 3 4 5 6 7 8 9 10)))))
