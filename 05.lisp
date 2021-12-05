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

;; (parseq:parseq 'd5-line "5,5 -> 8,2")

(defun read-input-d5 (path)
  (parseq:parseq 'd5-lines (uiop:read-file-string path)))

(defparameter in5 (read-input-d5 #P"./inputs/05.txt"))
(defparameter in5-test (read-input-d5 #P"./inputs/05-test.txt"))

(defun horizontal-p (a b)
  (= (first a) (first b)))

(defun vertical-p (a b)
  (= (second a) (second b)))

;; (defun loop-line (a b)
;;   "iterate over line coordinates, creating a 'bag' of points. (like python collections.Counter)"
;;   (let ((pts (fset:empty-bag)))
;;     (destructuring-bind (a-x a-y) a
;;       (destructuring-bind (b-x b-y) b
;; 	(let ((dx (signum (- b-x a-x)))
;; 	      (dy (signum (- b-y a-y)))
;; 	      (x a-x)
;; 	      (y a-y))
;; 	  (iter
;; 	    (setq pts (fset:with pts (cons x y)))
;; 	    (when (and (= x b-x) (= y b-y))
;; 	      (return))
;; 	    (incf x dx)
;; 	    (incf y dy)))))
;;     pts))

;; create clause for the (iter) macro
;; then we can simply (iter (for point in-line line) (do-whatever-with point))
(defmacro-clause (FOR point IN-LINE line)
  (alexandria:with-gensyms (line-val a-x a-y b-x b-y dx dy x y done) ; generate unique variable names, not to touch users variables
    `(progn
       (with ,line-val = ,line) 	; save the LINE expression into a variable, not to recompute its value multiple times
       (with ,a-x = (first (first ,line-val)))
       (with ,a-y = (second (first ,line-val)))
       (with ,b-x = (first (second ,line-val)))
       (with ,b-y = (second (second ,line-val)))
       (with ,dx = (signum (- ,b-x ,a-x)))
       (with ,dy = (signum (- ,b-y ,a-y)))
       (with ,x = ,a-x)
       (with ,y = ,a-y)
       (with ,done = nil)
       (for ,point next
	    (prog1 (cons ,x ,y)
	      (when ,done (terminate))
	      (when (and (= ,x ,b-x) (= ,y ,b-y)) (setq ,done t)) ; terminating immediatelly would trash the last point
	      (incf ,x ,dx)
	      (incf ,y ,dy))))))

;; (defun equivalent-loop-line (a b)
;;   (let ((point-bag (fset:empty-bag)))
;;     (iter (for point in-line (list a b))
;;       (setq point-bag (fset:with point-bag point)))
;;     point-bag))

(defun d5-p1 (&optional (inp in5))
  (let ((point-bag (fset:empty-bag))
	(n-dangerous 0))
    (iter (for (start end) in inp)
      (when (or (horizontal-p start end)
		(vertical-p start end))
	(iter (for point in-line (list start end))
	  (setq point-bag (fset:with point-bag
				     point)))))
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
      (iter (for point in-line (list start end))
	(setq point-bag (fset:with point-bag
				   point))))
    (fset:do-bag-pairs (coord count point-bag)
      (declare (ignore coord))
      (when (>= count 2)
	(incf n-dangerous)))
    n-dangerous))

;; (d5-p2 in5-test)
;; (d5-p2)
