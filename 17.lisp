(in-package #:advent-2021)

(defun read-input-d17 (path)
  (ppcre:register-groups-bind ((#'parse-integer min-x)
			       (#'parse-integer max-x)
			       (#'parse-integer min-y)
			       (#'parse-integer max-y))
      ("target area: x=(\\d+)..(\\d+), y=(-\\d+)..(-\\d+)" (uiop:read-file-string path))
    (list :min-x min-x :max-x max-x :min-y min-y :max-y max-y)))

(defparameter in17 (read-input-d17 #P"./inputs/17.txt"))
(defparameter in17-test (read-input-d17 #P"./inputs/17-test.txt"))

(defun d17-p1 (&optional (inp in17))
  (let ((max-last-y-step (- (getf inp :min-y))))
    (/ (* max-last-y-step (- max-last-y-step 1)) 2)))

;; (d17-p1 in17-test)
;; (d17-p1)

(defun inside-goal (x y goal)
  (and (<= (getf goal :min-x) x (getf goal :max-x))
       (<= (getf goal :min-y) y (getf goal :max-y))))

(defun past-goal (x y goal)
  (or (> x (getf goal :max-x))
      (< y (getf goal :min-y))))

(defun probe-reachable (x-vel y-vel goal)
  (iter
    (with x-vel = x-vel)
    (with y-vel = y-vel)
    (with x = 0)
    (with y = 0)

    (list x y)
    (incf x x-vel)
    (incf y y-vel)
    (decf y-vel 1)
    (decf x-vel (signum x-vel))

    (when (inside-goal x y goal)
      (return t))
    (when (past-goal x y goal)
      (return nil))))

(defun d17-p2 (&optional (inp in17))
  (let ((max-y-vel (- 0 (getf inp :min-y) 1))
	(min-y-vel (getf inp :min-y))
	(max-x-vel (getf inp :max-x))
	(min-x-vel (floor (sqrt (* (getf inp :min-x) 2)))))
    (length
     (iter outer (for y-vel from min-y-vel to max-y-vel)
       (iter (for x-vel from min-x-vel to max-x-vel)
	 (when (probe-reachable x-vel y-vel inp)
	   (in outer (collect (list x-vel y-vel)))))))))

;; (d17-p2 in17-test)
;; (d17-p2)

(def-suite day-17 :in all-tests)
(in-suite day-17)

(test d17-p1-example
  (is (= 45 (d17-p1 in17-test))))

(test d17-p2-example
  (is (= 112 (d17-p2 in17-test))))
