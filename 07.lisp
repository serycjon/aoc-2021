(in-package #:advent-2021)

(defun read-input-d7 (path)
  (mapcar #'parse-integer (uiop:split-string (uiop:stripln (uiop:read-file-string path))
					     :separator '(#\,))))

(defparameter in7 (read-input-d7 #P"./inputs/07.txt"))
(defparameter in7-test (read-input-d7 #P"./inputs/07-test.txt"))

(defun d7-p1 (&optional (inp in7))
  (let* ((len (length inp))
	 (median (nth (floor len 2) (sort (copy-list inp) #'<))))
    (reduce #'+
	    (mapcar (lambda (x) (abs (- median x))) inp)
	    :initial-value 0)))

;; (d7-p1 in7-test)
;; (d7-p1)

(defun crab-fuel-p2 (pos target)
  (mapcar (lambda (x)
	    (let ((n-steps (abs (- x target))))
	      (* (+ 1 n-steps) (/ n-steps 2))))
	  pos))

(defun sum (xs)
  (reduce #'+ xs :initial-value 0))

(defun d7-p2 (&optional (inp in7))
  (let* ((len (length inp))
	 (mean (/ (sum inp) len)))
    (values
     (min (sum (crab-fuel-p2 inp (floor mean)))
	  (sum (crab-fuel-p2 inp (ceiling mean))))
     ;; ^ some bruteforce stuff :/
     ;; we were optimizing L2 distance, not the real crab-fuel-distance
     mean)))

;; (d7-p2 in7-test)
;; (d7-p2)

;; (def-suite day-07 :in all-tests)
;; (in-suite day-07)

;; (test test-name
;;   (is (= 1 0)))


