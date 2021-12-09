(in-package #:advent-2021)

(defun pad-array (xs &optional (pad-val 10))
  (let* ((H (array-dimension xs 0))
	 (W (array-dimension xs 1))
	 (padded (make-array (list (+ H 2) (+ W 2)) :initial-element pad-val)))
    (iter (for row below H)
      (iter (for col below W)
	(setf (aref padded (1+ row) (1+ col))
	      (aref xs row col))))
    padded))

(defun lists-to-array (xs)
  (let ((H (length xs))
	(W (length (first xs))))
    (make-array (list H W) :initial-contents xs)))

(defun read-input-d9 (path)
  (lists-to-array
   (mapcar
    (lambda (line)
      (mapcar
       (lambda (c)
	 (- (char-int c) (char-int #\0)))
       (coerce line 'list)))
    (uiop:read-file-lines path))))

(defparameter in9 (read-input-d9 #P"./inputs/09.txt"))
(defparameter in9-test (read-input-d9 #P"./inputs/09-test.txt"))

(defun local-minimum-p (xs row col)
  (let ((query (aref xs row col))
	(top (aref xs (- row 1) col))
	(bottom (aref xs (+ row 1) col))
	(left (aref xs row (- col 1)))
	(right (aref xs row (+ col 1))))
    (< query (min top bottom left right))))

(defun d9-p1 (&optional (inp in9))
  (let ((padded (pad-array inp))
	(H (array-dimension inp 0))
	(W (array-dimension inp 1))
	(risks 0))
    (iter (for row below H)
      (iter (for col below W)
	(when (local-minimum-p padded (1+ row) (1+ col))
	  (incf risks (1+ (aref padded (1+ row) (1+ col)))))))
    risks))

;; (d9-p1 in9-test)
;; (d9-p1)

(defun get-smaller-neighbor (xs row col)
  (let ((query (aref xs row col))
	(top (list (- row 1) col))
	(bottom (list (+ row 1) col))
	(left (list row (- col 1)))
	(right (list row (+ col 1))))
    (flet ((val (coords) (aref xs (first coords) (second coords))))
      (cond
	((< (val top) query) top)
	((< (val bottom) query) bottom)
	((< (val left) query) left)
	((< (val right) query) right)
	(t nil)))))

(defun flow-down (xs row col)
  (when (>= (aref xs row col) 9)
    (return-from flow-down nil))
  (let ((downhill (get-smaller-neighbor xs row col)))
    (unless downhill (return-from flow-down (list row col)))
    (flow-down xs (first downhill) (second downhill))))

(defun d9-p2 (&optional (inp in9))
  (let ((padded (pad-array inp))
	(H (array-dimension inp 0))
	(W (array-dimension inp 1))
	(basins (fset:empty-bag)))
    (iter (for row below H)
      (iter (for col below W)
	(setq basins (fset:with basins (flow-down padded (1+ row) (1+ col))))))
    (let ((sizes))
      (fset:do-bag-pairs (coords count basins)
	(when coords
	  (push count sizes)))
      (setq sizes (sort sizes #'>))
      (* (first sizes) (second sizes) (third sizes)))))

;; (d9-p2 in9-test)
;; (d9-p2)

;; (def-suite day-09 :in all-tests)
;; (in-suite day-09)

;; (test test-name
;;   (is (= 1 0)))

