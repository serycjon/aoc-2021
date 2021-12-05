(in-package #:advent-2021)

(parseq:defrule d5-coord ()
    (and d4-number #\, d4-number)
  (:choose 0 2))

(parseq:defrule d5-line ()
    (and d5-coord " -> " d5-coord)
  (:choose 0 2))

(parseq:defrule d5-lines ()
    (+ (and d5-line #\Newline))
  (:lambda (&rest lines) (mapcar #'first lines)))

(parseq:parseq 'd5-line "5,5 -> 8,2")

(defun read-input-d5 (path)
  (parseq:parseq 'd5-lines (uiop:read-file-string path)))

(defparameter in5 (read-input-d5 #P"./inputs/05.txt"))
(defparameter in5-test (read-input-d5 #P"./inputs/05-test.txt"))

(defun horizontal-p (a b)
  (= (first a) (first b)))

(defun vertical-p (a b)
  (= (second a) (second b)))

(defun loop-line (a b)
  "iterate over line coordinates, creating a 'bag' of points. (like python collections.Counter)"
  (let ((pts (fset:empty-bag)))
    (destructuring-bind (a-x a-y) a
      (destructuring-bind (b-x b-y) b
	(let ((dx (signum (- b-x a-x)))
	      (dy (signum (- b-y a-y)))
	      (x a-x)
	      (y a-y))
	  (iter
	    (setq pts (fset:with pts (cons x y)))
	    (when (and (= x b-x) (= y b-y))
	      (return))
	    (incf x dx)
	    (incf y dy)))))
    pts))

(defun d5-p1 (&optional (inp in5))
  (let ((point-bag (fset:empty-bag))
	(n-dangerous 0))
    (iter (for (start end) in inp)
      (when (or (horizontal-p start end)
		(vertical-p start end))
	(setq point-bag (fset:bag-sum point-bag
				      (loop-line start end)))))
    (fset:do-bag-pairs (coord count point-bag)
      (declare (ignore coord))
      (when (>= count 2)
	(incf n-dangerous)))
    n-dangerous))

;; (d5-p1 in5-test)
;; (d5-p1)

(defun d5-p2 (&optional (inp in5))
  (let ((point-bag (fset:empty-bag))
	(n-dangerous 0))
    (iter (for (start end) in inp)
      (setq point-bag (fset:bag-sum point-bag
				    (loop-line start end))))
    (fset:do-bag-pairs (coord count point-bag)
      (declare (ignore coord))
      (when (>= count 2)
	(incf n-dangerous)))
    n-dangerous))

;; (d5-p2 in5-test)
;; (d5-p2)
