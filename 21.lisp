(in-package #:advent-2021)

(defun read-input-d21 (path)
  (mapcar (lambda (line)
	    (parse-integer (car (last (uiop:split-string line)))))
	  (uiop:read-file-lines path)))

(defparameter in21 (read-input-d21 #P"./inputs/21.txt"))
(defparameter in21-test (read-input-d21 #P"./inputs/21-test.txt"))

(defun mod1 (x modulo)
  "1-based indexing modulo"
  (1+ (mod (- x 1) modulo)))

(defun d21-p1 (&optional (inp in21))
  (let ((die-state 1)
	(n-rolls 0)
	(p-1-pos (first inp))
	(p-2-pos (second inp))
	(p-1-points 0)
	(p-2-points 0))
    (flet ((roll-die ()
	     (prog1 die-state
	       (setf die-state (mod1 (1+ die-state) 10))
	       (incf n-rolls))))
      (iter
	;; (format t "~a: ~a ~a~%" n-rolls p-1-points p-2-points)

	(iter (for roll-i below 3)
	  (for roll = (roll-die))
	  (setf p-1-pos (mod1 (+ p-1-pos roll) 10)))
	(incf p-1-points p-1-pos)
	(when (>= p-1-points 1000) (return (* n-rolls p-2-points)))

	(iter (for roll-i below 3)
	  (for roll = (roll-die))
	  (setf p-2-pos (mod1 (+ p-2-pos roll) 10)))
	(incf p-2-points p-2-pos)
	(when (>= p-2-points 1000) (return (* n-rolls p-1-points)))))))

;; (d21-p1 in21-test)
;; (d21-p1)

(defun all-dirac-3rolls ()
 (let ((sums (fset:empty-bag)))
   (iter (for a from 1 to 3)
     (iter (for b from 1 to 3)
       (iter (for c from 1 to 3)
	 (fset:adjoinf sums (+ a b c)))))
   sums))

(defparameter *dirac-3rolls*
  (make-array 7 :initial-contents '(3 4 5 6 7 8 9)))
(defparameter *dirac-3roll-counts*
  (make-array 7 :initial-contents '(1 3 6 7 6 3 1)))

(defun propagate-state (state p1-turn)
  (let ((new-state (make-array (array-dimensions state))))
    (flet ((propagate-single (p1-score p1-pos-i p2-score p2-pos-i)
	     (let ((cur-count (aref state p1-score p1-pos-i p2-score p2-pos-i))
		   (cur-p1-pos (1+ p1-pos-i))
		   (cur-p2-pos (1+ p2-pos-i)))
	       (unless (= 0 cur-count)
		 (iter
		   (for roll-sum in-vector *dirac-3rolls*)
		   (for roll-count in-vector *dirac-3roll-counts*)
		   (let* ((new-p1-pos (if p1-turn (mod1 (+ cur-p1-pos roll-sum) 10) cur-p1-pos))
			  (new-p2-pos (if p1-turn cur-p2-pos (mod1 (+ cur-p2-pos roll-sum) 10)))
			  (new-p1-pos-i (- new-p1-pos 1))
			  (new-p2-pos-i (- new-p2-pos 1))
			  (new-p1-score (if p1-turn (+ p1-score new-p1-pos) p1-score))
			  (new-p2-score (if p1-turn p2-score (+ p2-score new-p2-pos))))
		     (incf (aref new-state new-p1-score new-p1-pos-i new-p2-score new-p2-pos-i) (* roll-count cur-count))))))))
      (iter (for p1-score from 0 below 21)
	(iter (for p1-pos-i below 10)
	  (iter (for p2-score from 0 below 21)
	    (iter (for p2-pos-i below 10)
	      (propagate-single p1-score p1-pos-i p2-score p2-pos-i)))))
      new-state)))

(defun count-and-terminate-wins (state)
  (let ((p1-wins 0)
	(p2-wins 0))
    ;; games won by p1
    (iter (for p1-score from 21 below 32)
      (iter (for p1-pos-i below 10)
	(iter (for p2-score below 32)
	  (iter (for p2-pos-i below 10)
	    (incf p1-wins (aref state p1-score p1-pos-i p2-score p2-pos-i))
	    (setf (aref state p1-score p1-pos-i p2-score p2-pos-i) 0)))))

    ;; games won by p2
    (iter (for p1-score below 32)
      (iter (for p1-pos-i below 10)
	(iter (for p2-score from 21 below 32)
	  (iter (for p2-pos-i below 10)
	    (incf p2-wins (aref state p1-score p1-pos-i p2-score p2-pos-i))
	    (setf (aref state p1-score p1-pos-i p2-score p2-pos-i) 0)))))
    (values p1-wins p2-wins)))

(defun d21-p2 (&optional (inp in21))
  (let ((state (make-array (list 32 10 32 10)))
	(p1-turn t)
	(p1-wins 0)
	(p2-wins 0))
    (setf (aref state 0 (- (first inp) 1) 0 (- (second inp) 1)) 1)
    (iter (for i below 21)
      (setq state (propagate-state state p1-turn)
	    p1-turn (not p1-turn))
      (multiple-value-bind (new-p1-wins new-p2-wins) (count-and-terminate-wins state)
	(incf p1-wins new-p1-wins)
	(incf p2-wins new-p2-wins)))
    (max p1-wins p2-wins)))

;; (d21-p2 in21-test)
;; (d21-p2)

(def-suite day-21 :in all-tests)
(in-suite day-21)

(test d21-p1-example
  (is (= 739785 (d21-p1 in21-test))))

(test d21-p2-example
  (is (= 444356092776315 (d21-p2 in21-test))))
