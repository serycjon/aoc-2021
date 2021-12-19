(in-package #:advent-2021)

(parseq:defrule number-with-sign ()
    (and (? (or "+" "-"))
	 (+ digit))
  (:string)
  (:function #'parse-integer))

(parseq:defrule thing-followed-by (thing sep)
    (and thing (? sep))
  (:choose 0))

(parseq:defrule scanner-header ()
    (and "--- scanner " d4-number " ---" #\Newline)
  (:choose 1))

(parseq:defrule scan ()
    (and number-with-sign "," number-with-sign "," number-with-sign)
  (:choose 0 2 4))

(parseq:defrule scanner ()
    (and scanner-header (+ (thing-followed-by scan #\Newline))))

(parseq:defrule d19 ()
    (+ (thing-followed-by scanner #\Newline)))

(defun read-input-d19 (path)
  (parseq:parseq 'd19 (uiop:read-file-string path)))

;; (parseq:trace-rule 'd19 :recursive t)

(defparameter in19 (read-input-d19 #P"./inputs/19.txt"))
(defparameter in19-test (read-input-d19 #P"./inputs/19-test.txt"))

(defparameter *dir-names* (list 'x 'y 'z 'nx 'ny 'nz))

(defun name2direction (name)
  (ecase name
    (x (list 1 0 0))
    (y (list 0 1 0))
    (z (list 0 0 1))
    (nx (list -1 0 0))
    (ny (list 0 -1 0))
    (nz (list 0 0 -1))))

(defun rot-matrix (a b c)
  (let ((R (make-array '(3 3) :initial-element 0)))
    (iter
      (for i from 0)
      (for x in (name2direction a))
      (setf (aref R 0 i) x))

    (iter
      (for i from 0)
      (for x in (name2direction b))
      (setf (aref R 1 i) x))

    (iter
      (for i from 0)
      (for x in (name2direction c))
      (setf (aref R 2 i) x))
    R))

(defun all-rot-matrices ()
  (iter outer (for a in *dir-names*)
    (iter (for b in *dir-names*)
      (iter (for c in *dir-names*)
	(let ((R (rot-matrix a b c)))
	  (when (pure-rot-p R)
	    (in outer (collect R))))))))

(defparameter *all-R-mats* (all-rot-matrices))

(assert (= (length *all-R-mats*) 24))

(defun pure-rot-p (M)
  (= 1 (determinant-3x3 M)))

(defun determinant-3x3 (M)
  (- (+ (* (aref M 0 0) (aref M 1 1) (aref M 2 2))
	(* (aref M 1 0) (aref M 2 1) (aref M 0 2))
	(* (aref M 2 0) (aref M 0 1) (aref M 1 2)))
     (+ (* (aref M 0 2) (aref M 1 1) (aref M 2 0))
	(* (aref M 1 2) (aref M 2 1) (aref M 0 0))
	(* (aref M 2 2) (aref M 0 1) (aref M 1 0)))))

(defun rotate-pt (R pt)
  (iter (for i below 3)
    (collect
	(iter
	  (for j below 3)
	  (for x in pt)
	  (sum (* (aref R i j) x))))))

(defun rotate-pts (R pts)
  (mapcar (lambda (pt) (rotate-pt R pt))
	  pts))

(defun shift-pts (shift pts)
  (mapcar
   (lambda (pt) (mapcar #'+ pt shift))
   pts))

(defun compute-overlap (xs ys)
  (let ((intersection (fset:intersection (list-to-set xs)
					 (list-to-set ys))))
    (values (fset:size intersection) intersection)))

(defun align-scanners (fixed new)
  "attempt to align the new scanner with the fixed scanner"
  (iter (for R in *all-R-mats*)
    (let ((new-rot (rotate-pts R new)))
      (iter (for fixed-beacon in fixed)
	(iter (for new-beacon in new-rot)
	  (let* ((shift (mapcar #'- fixed-beacon new-beacon))
		 (new-rot-shift (shift-pts shift new-rot)))
	    (multiple-value-bind (overlap overlap-set) (compute-overlap fixed new-rot-shift)
	      (when (>= overlap 12)
		(return-from align-scanners (list new-rot-shift overlap-set R shift))))))))))

(defun count-beacons (aligned-scans)
  (fset:size (reduce #'fset:union (mapcar #'list-to-set aligned-scans))))

(defun d19-p1 (&optional (inp in19))
  (let ((scans (copy-tree (mapcar #'second inp)))
	(oriented (list 0)))
    (iter
      (iter
	(for fixed-i in oriented)
	(for fixed = (nth fixed-i scans))
	(iter
	  (for new-i below (length scans))
	  (for new = (nth new-i scans))
	  (unless (member new-i oriented)
	    (let ((aligned (align-scanners fixed new)))
	      (when aligned
		;; (format t "aligned ~a to ~a~%" new-i fixed-i)
		(setf (nth new-i scans) (first aligned))
		(push new-i oriented))))))
      (when (= (length oriented) (length scans))
	(return)))
    (count-beacons scans)))

;; (d19-p1 in19-test)
;; (d19-p1)

(defun manhattan-dist (a b)
  (reduce #'+ (mapcar #'abs (mapcar #'- a b))))

(defun max-manhattan-dist (positions)
  (iter (for a in positions)
    (maximize (iter (for b in positions)
		(maximize (manhattan-dist a b))))))

(defun d19-p2 (&optional (inp in19))
  (let ((scans (copy-tree (mapcar #'second inp)))
	(oriented (list 0))
	(scanner-positions '((0 0 0))))
    (iter
      (iter
	(for fixed-i in oriented)
	(for fixed = (nth fixed-i scans))
	(iter
	  (for new-i below (length scans))
	  (for new = (nth new-i scans))
	  (unless (member new-i oriented)
	    (let ((aligned (align-scanners fixed new)))
	      (when aligned
		;; (format t "aligned ~a to ~a~%" new-i fixed-i)
		(setf (nth new-i scans) (first aligned))
		(push new-i oriented)
		(push (fourth aligned) scanner-positions))))))
      (when (= (length oriented) (length scans))
	(return)))
    (max-manhattan-dist scanner-positions)))

;; (d19-p2 in19-test)
;; (d19-p2)

(def-suite day-19 :in all-tests)
(in-suite day-19)

;; slow
;; (test d19-p1-example
;;   (is (= 79 (d19-p1 in19-test))))

;; slow
;; (test d19-p2-example
;;   (is (= 3621 (d19-p2 in19-test))))
