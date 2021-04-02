(defpackage :tacitus
  (:use :cl :monad)
  (:export :between :to-array :append-ranges :reduce-range :index :memoization
           :size
           :fmap
           :flatmap
           :fmap-rec))

(in-package :tacitus)

(defstruct range
  (size 0 :type integer)
  (transformation #'identity :type function)
  (memoization nil))

(defun between (start end &key (memoize nil))
  (make-range :size (+ (- end start) 1) 
              :transformation (lambda (i) (+ i start))
              :memoization (when memoize (lambda (i) (+ i start)))))

(defun to-array (range &key (type t))
  (let ((arr (make-array (range-size range) :element-type type :adjustable nil)))
    (loop for i from 0 to (- (range-size range) 1)
       do (setf (aref arr i) (index range i)))
    arr))

(defun append-ranges (one other)
  (declare (optimize (speed 3)))
  (let* ((one-f (range-transformation one))
         (other-f (range-transformation other))
         (one-size (range-size one))
         (new-f (lambda (i) 
                  (if (>= i one-size)
                      (funcall other-f (- i one-size))
                      (funcall one-f i)))))
    (declare (function one-f other-f new-f))
    (make-range :size (+ (range-size one) (range-size other)) :transformation new-f)))

(defmacro fmap-rec (f range)
  (let ((self (intern "SELF" (package-name *package*))))
    `(let (,self)
       (setf ,self (fmap ,f ,range)))))

(defmethod fmap (f (range range))
  (declare (optimize (speed 3))
           (function f))
  (let ((old-f (range-transformation range)))
    (declare (function old-f))
    (make-range :size (range-size range) 
                   :transformation (lambda (i) (funcall f (funcall old-f i)))
                   :memoization (when (range-memoization range)
                                  (let* ((table (make-hash-table :test 'equal)))
                                    (lambda (i)
                                      (if (gethash i table)
                                          (gethash i table)
                                          (setf (gethash i table) 
                                                (funcall f (funcall old-f i))))))))))

(defun merge-ranges (f range start end)
  (declare (optimize (speed 3)))
  (if (= start end)
      (let ((old-f (range-transformation range)))
        (declare (function f old-f))
        (funcall f (funcall old-f start)))
      (let* ((mid (floor (+ start end) 2))
             (left (merge-ranges f range start mid))
             (right (merge-ranges f range (+ mid 1) end)))
        (append-ranges left right))))

(defmethod flatmap (f (range range))
  (merge-ranges f range 0 (- (range-size range) 1)))

(defun index (range i)
  (when (< i (range-size range))
    (if (range-memoization range)
        (funcall (range-memoization range) i)
        (funcall (range-transformation range) i))))

(defun reduce-range (op range)
  (declare (optimize (speed 3))
           (function op))
  (let ((len  (- (range-size range) 1))
        (f (if (range-memoization range)
               (range-memoization range)
               (range-transformation range))))
    (declare (function f))
    (loop for i fixnum from 0 to len
       for e = (funcall f i)
       for result = e then (funcall op result e)
       finally (return result))))
