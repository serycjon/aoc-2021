(in-package #:advent-2021)

(defun read-input (path)
  (mapcar #'parse-integer (uiop:read-file-lines path)))

(defparameter in1 (read-input #P"./inputs/01-1.txt"))

(defun n-increases (xs)
 (iter
   (for current in xs)
   (for previous :previous current :initially (first xs))
   (counting (> current previous))))

(defun sum-groups-slow (xs group-sz)
  (let* ((len (length xs))
	 (n-groups (* group-sz (floor len group-sz))))
    (iter (for group-i below n-groups)
      (collecting (iter (for i below group-sz)
		    (summing (nth (+ group-i i) xs)))))))

(defun sum-first-n (xs n)
  (iter
    (with sum = 0)
    (with n-summed = 0)
    (for i below n)
    (for x in xs)
    (setq sum (+ sum x)
	  n-summed (+ n-summed 1))
    (finally
     (return
      (if (= n-summed n)
	  sum
	  nil)))))

(defun sum-groups (xs group-sz)
  (iter
    (for sub-xs on xs)
    (let ((group-sum (sum-first-n sub-xs group-sz)))
      (when group-sum (collect group-sum)))))

(defun d1-p1 ()
 (let ((inp in1))
   (n-increases inp)))

(defun d1-p2 ()
  (let ((inp in1))
    (n-increases (sum-groups inp 3))))

(def-suite all-tests)

(def-suite day-01 :in all-tests)
(in-suite day-01)

(test group-sums
  (is (equal
       (sum-groups (read-input #P"./inputs/01-test.txt") 3)
       (list 607 618 618 617 647 716 769 792))))

(defun run-tests ()
  (run! 'all-tests))
