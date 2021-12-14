(in-package #:advent-2021)

(defun read-input-d14 (path)
  (let ((lines (uiop:read-file-lines path)))
    (list (coerce (first lines) 'list)
	  (reduce
	   #'fset:map-union
	   (mapcar (lambda (line)
		     (ppcre:register-groups-bind (src dst)
			 ("(\\w+) -> (\\w+)" line)
		       (fset:with (fset:empty-map) (coerce src 'list) (uiop:first-char dst))))
		   (cddr lines))))))

(defparameter in14 (read-input-d14 #P"./inputs/14.txt"))
(defparameter in14-test (read-input-d14 #P"./inputs/14-test.txt"))

(defun grow-polymer (current rules)
  (iter
    (with new = nil)
    (for B in current)
    (for A previous B)
    (when A
      (push A new)
      (push (fset:lookup rules (list A B)) new))
    (finally (progn (push B new)
		    (return (nreverse new))))))

(defun list-to-bag (xs)
  (iter
    (with bag = (fset:empty-bag))
    (for x in xs)
    (fset:adjoinf bag x)
    (finally (return bag))))

(defun d14-p1 (&optional (inp in14))
  (let ((current (copy-list (first inp))))
    (iter (for i below 10)
      (setq current (grow-polymer current (second inp))))
    (let ((max 0)
	  (min 1e20))
      (fset:do-bag-pairs (char count (list-to-bag current))
	(declare (ignore char))
	(when (> count max) (setq max count))
	(when (< count min) (setq min count)))
      (- max min))))

;; (d14-p1 in14-test)
;; (d14-p1)

(defun grow-polymer-pair-counts (pair rules)
  (let ((inserted (fset:lookup rules pair)))
    (values
     (fset:bag (list (first pair) inserted) (list inserted (second pair))) ; keeping track of pair counts
     (fset:bag inserted) 		; keeping track of each letter count
     )))

(defun bag-multiply (bag times)
  ;; had to read the fset source code
  ;; https://github.com/slburson/fset/blob/69c209e6eb15187da04f70ece3f800a6e3cc8639/Code/fset.lisp#L1455
  (let ((orig-wb-bag-contents (fset::wb-bag-contents bag)))
    (fset::make-wb-bag (cons (car orig-wb-bag-contents)
			     (iter (for x in-vector (cdr orig-wb-bag-contents))
			       (collecting (* times x) result-type vector))))))

(defun step-polymer-compressed (single-counts pair-counts rules)
  (let ((new-single single-counts)
	(new-pairs (fset:empty-bag)))
    (fset:do-bag-pairs (pair pair-count pair-counts)
      (multiple-value-bind (pair-bag single-bag) (grow-polymer-pair-counts pair rules)
	(setq new-single (fset:bag-sum new-single (bag-multiply single-bag pair-count)))
	(setq new-pairs (fset:bag-sum new-pairs (bag-multiply pair-bag pair-count)))))
    (values new-single new-pairs)))

(defun d14-p2 (&optional (inp in14) (steps 40))
  (let ((single-counts (fset:empty-bag))
	(pair-counts (fset:empty-bag))
	(rules (second inp)))
    ;; the idea is to only keep track of number of letter pairs + number of each letter
    ;; initial setup:
    (iter
      (for B in (first inp))
      (for A previous B)
      (fset:adjoinf single-counts B)
      (when A
	(fset:adjoinf pair-counts (list A B))))

    ;; polymer growing
    (iter (for i below steps)
      (multiple-value-bind (new-single new-pair) (step-polymer-compressed single-counts pair-counts rules)
	  (setq single-counts new-single
		pair-counts new-pair)))

    ;; final result calculation
    (let ((max 0)
	  (min 1e20))
      (fset:do-bag-pairs (char count single-counts)
	(declare (ignore char))
	(when (> count max) (setq max count))
	(when (< count min) (setq min count)))
      (- max min))))

;; (d14-p2 in14-test)
;; (d14-p2)

(def-suite day-14 :in all-tests)
(in-suite day-14)

(test d14-p1-example
  (is (= (d14-p1 in14-test) 1588)))

(test d14-p2-example
  (is (= (d14-p2 in14-test) 2188189693529)))
