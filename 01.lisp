(in-package #:advent-2021)

(defparameter in1 (mapcar #'parse-integer (uiop:read-file-lines #P"./inputs/01-1.txt")))

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

(let ((inp in1))
  (n-increases inp))

(let ((inp in1))
  (n-increases (sum-groups inp 3)))
