(in-package #:advent-2021)

(parseq:defrule comma-complex-coord ()
    (and d4-number #\, d4-number)
  (:choose 0 2)
  (:lambda (x y) (complex x y)))

(parseq:defrule d13-dots ()
    (+ (and comma-complex-coord #\Newline))
  (:lambda (&rest lines) (mapcar #'first lines)))

;; parsing "fold along x=17" will return a function that performs the folding operation
(parseq:defrule d13-fold ()
    (and "fold along " (char "xy") "=" d4-number)
  (:choose 1 3)
  (:lambda (axis fold-coord)
    (if (char= axis #\x)
     (lambda (coords)
       (complex
	(min (realpart coords) (- fold-coord (- (realpart coords) fold-coord)))
	(imagpart coords)))
     (lambda (coords)
       (complex
	(realpart coords)
	(min (imagpart coords) (- fold-coord (- (imagpart coords) fold-coord))))))))

(parseq:defrule d13-folds ()
    (+ (and d13-fold #\Newline))
  (:lambda (&rest lines) (mapcar #'first lines)))

(parseq:defrule d13 ()
    (and d13-dots #\Newline d13-folds)
  (:choose 0 2))

(defun read-input-d13 (path)
  (parseq:parseq 'd13 (uiop:read-file-string path)))

(defparameter in13 (read-input-d13 #P"./inputs/13.txt"))
(defparameter in13-test (read-input-d13 #P"./inputs/13-test.txt"))

(defun count-unique (xs)
  (iter
    (with set = (fset:empty-set))
    (for x in xs)
    (fset:adjoinf set x)
    (finally (return (fset:size set)))))

(defun d13-p1 (&optional (inp in13))
  (let ((coords (copy-list (first inp)))
	(foldings (second inp)))
    (iter (for folding in foldings)
      (setq coords
	    (mapcar folding coords))
      (return nil))
    (count-unique coords)))

;; (d13-p1 in13-test)
;; (d13-p1)

(defun draw-coords-grid (coords)
  (destructuring-bind (H W) (iter (for x in coords)
			      ;; find the result dimensions
			      (maximize (realpart x) into W)
			      (maximize (imagpart x) into H)
			      (finally (return (list H W))))
    (let ((grid (make-array (list (1+ H) (1+ W)) :initial-element #\Space)))
      (iter (for coord in coords)
	;; mark the coords with unicode FULL_BLOCK
	(setf (aref grid (imagpart coord) (realpart coord)) #\FULL_BLOCK))

      ;; convert to list of strings
      (iter (for row below (array-dimension grid 0))
	(collecting
	 (coerce
	  (iter (for col below (array-dimension grid 1))
	    (collect (aref grid row col)))
	  'string))))))

(defun d13-p2 (&optional (inp in13))
  (let ((coords (copy-list (first inp)))
	(foldings (second inp)))
    (iter (for folding in foldings)
      (setq coords
	    (mapcar folding coords)))
    (draw-coords-grid coords)))

;; (d13-p2 in13-test)
;; (d13-p2)
;; ("███  █    █  █   ██ ███  ███   ██   ██ "
;;  "█  █ █    █ █     █ █  █ █  █ █  █ █  █"
;;  "███  █    ██      █ █  █ ███  █  █ █   "
;;  "█  █ █    █ █     █ ███  █  █ ████ █ ██"
;;  "█  █ █    █ █  █  █ █ █  █  █ █  █ █  █"
;;  "███  ████ █  █  ██  █  █ ███  █  █  ███")

(def-suite day-13 :in all-tests)
(in-suite day-13)

(test d13-p1-example
  (is (= (d13-p1 in13-test) 17)))
