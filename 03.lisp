(in-package #:advent-2021)

(defun parse-line-d3 (line)
  (mapcar (lambda (c)
	    (- (char-code c)
	       (char-code #\0)))
	  (coerce line 'list)))

(defun read-input-d3 (path)
  (mapcar #'parse-line-d3 (uiop:read-file-lines path)))

(defun sum-cols (list-of-lists)
  (let* ((n-cols (length (first list-of-lists)))
	 (accumulator (iter (for i below n-cols)
			(collect 0))))
    (iter (for row in list-of-lists)
      (iter
	(for x in row)
	(for acc-val on accumulator)
	(incf (car acc-val) x)))
    accumulator))

(defun mean-bit (inp)
  (mapcar (lambda (x) (/ x (length inp)))
	  (sum-cols inp)))

(defparameter in3 (read-input-d3 #P"./inputs/03.txt"))

(defparameter in3-test (read-input-d3 #P"./inputs/03-test.txt"))

(defun d3-p1 (&optional (inp in3))
  (let* ((mean-bit (mean-bit inp))
	 (gamma-str (map 'string (lambda (x) (if (> x 0.5) #\0 #\1))
			 mean-bit))
	 (epsilon-str (map 'string (lambda (x) (if (< x 0.5) #\0 #\1))
			   mean-bit)))
    (* (parse-integer gamma-str :radix 2)
       (parse-integer epsilon-str :radix 2))))

(defun criterium (xs bit-pos &optional (least-common nil))
  (let* ((mean-bit (mean-bit xs)))
    (if (if least-common
	    (not (>= (nth bit-pos mean-bit) 1/2))
	    (>= (nth bit-pos mean-bit) 1/2))
	1 0)))

(defun filter-numbers (inp &optional (least-common nil))
  (let ((xs (copy-list inp)))
    (iter
     (for i below (length (first xs)))
      (let ((crit (criterium xs i least-common)))
	(setq xs (remove-if-not (lambda (x) (= (nth i x) crit))
				xs))
	)
     (when (= 1 (length xs))
       (return (first xs))))))

(defun to-binary (xs)
  (map 'string (lambda (x) (if (> x 0) #\1 #\0))
       xs))

(defun d3-p2 (&optional (inp in3))
  (* (parse-integer (to-binary (filter-numbers inp)) :radix 2)
     (parse-integer (to-binary (filter-numbers inp t)) :radix 2)))

;; (d3-p1)
;; (d3-p2)

(def-suite day-03 :in all-tests)
(in-suite day-03)

(test filter-numbers-most
  (is (equal
       "10111"
       (to-binary (filter-numbers in3-test)))))

(test filter-numbers-least
  (is (equal
       "01010"
       (to-binary (filter-numbers in3-test t)))))

(test example
  (is (= 230 (d3-p2 in3-test))))
