(in-package #:advent-2021)

(defun read-input-d11 (path)
  (read-input-d9 path))

(defparameter in11 (read-input-d11 #P"./inputs/11.txt"))
(defparameter in11-test (read-input-d11 #P"./inputs/11-test.txt"))

(defun d11-step (energy-levels)
  (let ((to-propagate))
    (destructuring-bind (H W) (array-dimensions energy-levels)
      ;; initial increment
      (iter (for r below H)
	(iter (for c below W)
	  (incf (aref energy-levels r c))
	  (when (> (aref energy-levels r c) 9)
	    (push (cons r c) to-propagate)))))

    ;; propagation
    (iter (for (r . c) = (pop to-propagate))
      (when (null r) (return))
      (iter (for prop-r from (- r 1) to (+ r 1))
	(iter (for prop-c from (- c 1) to (+ c 1))
	  (when (array-in-bounds-p energy-levels prop-r prop-c)
	    (incf (aref energy-levels prop-r prop-c))
	    (when (= (aref energy-levels prop-r prop-c) 10)
	      (push (cons prop-r prop-c) to-propagate))))))

    ;; cleanup
    (iter (for i below (array-total-size energy-levels))
      (when (> (row-major-aref energy-levels i) 9)
	(setf (row-major-aref energy-levels i) 0)))
    energy-levels))

(defun count-flashes (energy-levels)
  (iter (for i below (array-total-size energy-levels))
    (count (= 0 (row-major-aref energy-levels i)))))

(defun d11-p1 (&optional (inp in11))
  (let ((levels (alexandria:copy-array inp))
	(flashes 0))
    (iter (for step from 1 to 100)
      (incf flashes (count-flashes (d11-step levels))))
    flashes))

;; (d11-p1 in11-test)
;; (d11-p1)

(defun d11-p2 (&optional (inp in11))
  (let ((levels (alexandria:copy-array inp)))
    (iter (for step from 1)
      (when (= (array-total-size levels) (count-flashes (d11-step levels)))
	(return step)))))

;; (d11-p2 in11-test)
;; (d11-p2)

(def-suite day-11 :in all-tests)
(in-suite day-11)

(test d11-example-p1
  (is (= (d11-p1 in11-test) 1656)))

(test d11-example-p2
  (is (= (d11-p2 in11-test) 195)))
