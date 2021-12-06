(in-package #:advent-2021)

(defun read-input-d6 (path)
  (mapcar #'parse-integer
	  (uiop:split-string
	   (uiop:stripln (uiop:read-file-string path))
	   :separator '(#\,))))

(defparameter in6 (read-input-d6 #P"./inputs/06.txt"))
(defparameter in6-test (read-input-d6 #P"./inputs/06-test.txt"))

(defun step-one-day (counts)
  (let ((n-zero (aref counts 0)))
    (dotimes (i 8)
      (setf (aref counts i)
	    (aref counts (1+ i))))
    (incf (aref counts 6) n-zero)
    (setf (aref counts 8) n-zero)
    counts))

(defun d6-p1 (&optional (inp in6))
  (let ((fish (copy-list inp))
	(age-counts (make-array 9 :initial-element 0)))
    (iter (for age in fish)
      (incf (aref age-counts age)))
    (dotimes (day 80)
      (step-one-day age-counts))
    (iter (for x in-vector age-counts)
      (summing x))))

;; (d6-p1 in6-test)
;; (d6-p1)

(defun d6-p2 (&optional (inp in6))
  (let ((fish (copy-list inp))
	(age-counts (make-array 9 :initial-element 0)))
    (iter (for age in fish)
      (incf (aref age-counts age)))
    (dotimes (day 256)
      (step-one-day age-counts))
    (iter (for x in-vector age-counts)
      (summing x))))

;; (d6-p2 in6-test)
;; (d6-p2)
