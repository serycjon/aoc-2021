(in-package #:advent-2021)

(defun read-input-d15 (path)
  (read-input-d9 path))

(defparameter in15 (read-input-d15 #P"./inputs/15.txt"))
(defparameter in15-test (read-input-d15 #P"./inputs/15-test.txt"))

(defun DP-shortest-path (costs)
  (let ((padded (pad-array costs 100000000))
	(prev-coords (make-array (array-dimensions costs) :initial-element (complex 0 0)))
	(H (array-dimension costs 0))
	(W (array-dimension costs 1)))
    (setf (aref padded 1 1) 0)		; the start is not counted
    (setf (aref padded 0 1) 0)
    (setf (aref padded 1 0) 0)
    (iter (for row below H)
      (iter (for col below W)
	(let ((from-top (aref padded (- (1+ row) 1) (1+ col)))
	      (from-left (aref padded (1+ row) (- (1+ col) 1))))
	  (if (< from-top from-left)
	      (progn (incf (aref padded (1+ row) (1+ col)) from-top)
		     (setf (aref prev-coords row col) (complex (- row 1) col)))
	      (progn (incf (aref padded (1+ row) (1+ col)) from-left)
		     (setf (aref prev-coords row col) (complex row (- col 1))))))))
    (aref padded H W)))

(defun d15-p1 (&optional (inp in15))
  (shortest-path inp))

;; (d15-p1 in15-test)
;; (d15-p1)

(defun risk-wrap (val)
  (+ 1
     (mod (- val 1) 9)))

(defun expand-cave (costs &optional (times 5))
  (let* ((cell-H (array-dimension costs 0))
	 (cell-W (array-dimension costs 1))
	 (H (* times cell-H))
	 (W (* times cell-W))
	 (result (make-array (list H W) :initial-element 0)))
    (iter (for cell-row below times)
      (iter (for cell-col below times)
	(iter (for row below (array-dimension costs 0))
	  (iter (for col below (array-dimension costs 1))
	    (setf (aref result
			(+ (* cell-row cell-H) row)
			(+ (* cell-col cell-W) col))
		  (risk-wrap
		   (+ (aref costs row col)
		      cell-row cell-col)))))))
    result))

(defun shortest-path (costs)
  (let ((queue (priority-queue:make-pqueue #'<))
	(goal (complex (- (array-dimension costs 0) 1)
		       (- (array-dimension costs 1) 1)))
	(deltas (list (complex -1 0) (complex 1 0) (complex 0 -1) (complex 0 1)))
	(closed (make-array (array-dimensions costs) :initial-element nil))
	(path-costs (make-array (array-dimensions costs) :initial-element 100000)))

    ;; init
    (priority-queue:pqueue-push (complex 0 0) 0 queue)
    (setf (aref path-costs 0 0) 0)

    (iter
      ;; (for i below 2)
      (for (coord path-cost) = (multiple-value-list (priority-queue:pqueue-pop queue)))
      ;; (list (realpart coord) (imagpart coord) path-cost)
      (when (= goal coord) (return path-cost))
      (unless (aref closed (realpart coord) (imagpart coord))
	(iter (for delta in deltas)
	  (let* ((neighbor (+ coord delta))
		 (n-row (realpart neighbor))
		 (n-col (imagpart neighbor)))
	    (when (and (array-in-bounds-p costs n-row n-col)
		       (not (aref closed n-row n-col)))
	      (let ((neigh-path-cost (+ path-cost (aref costs n-row n-col))))
		(when (< neigh-path-cost (aref path-costs n-row n-col))
		  (priority-queue:pqueue-push neighbor neigh-path-cost queue)
		  (setf (aref path-costs n-row n-col) neigh-path-cost))))))

	(setf (aref closed (realpart coord) (imagpart coord)) t)))))

(defun d15-p2 (&optional (inp in15))
  ;; FFS, I did not read the task description carefully, thought you can only go right and down
  ;; unfortunately all the examples worked with right and down :/
  ;; (DP-shortest-path (expand-cave inp))
  (shortest-path (expand-cave inp))
  )

;; (d15-p2 in15-test)
;; (d15-p2)

(def-suite day-15 :in all-tests)
(in-suite day-15)

(test d15-p1-example
  (is (= (d15-p1 in15-test) 40)))

(defun array-equal (a b)
  (and (equal (array-dimensions a) (array-dimensions b))
       (iter (for i below (array-total-size a))
	 (always (equal (row-major-aref a i)
			(row-major-aref b i))))))

(test d15-p2-expand
  (is (array-equal (read-input-d15 #P"./inputs/15-test-expanded.txt") (expand-cave in15-test))))

(test d15-p2-example
  (is (= (d15-p2 in15-test) 315)))
