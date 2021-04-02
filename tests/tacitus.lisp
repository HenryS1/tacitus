(defpackage tacitus/tests/tacitus
  (:use :cl
        :tacitus
        :rove))
(in-package :tacitus/tests/tacitus)

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
