(in-package #:advent-2021)

(defun list-to-set (xs)
  (iter
    (with res = (fset:empty-set))
    (for x in xs)
    (setq res (fset:with res x))
    (finally (return res))))

(defun read-7segment-code (str)
  (list-to-set (coerce str 'list)))

(defun read-input-d8 (path)
  (mapcar (lambda (line)
	    (mapcar
	     (lambda (part)
	       (mapcar #'read-7segment-code (uiop:split-string (string-trim " " part))))
	     (uiop:split-string line :separator '(#\|))))
	  (uiop:read-file-lines path)))

(defparameter in8 (read-input-d8 #P"./inputs/08.txt"))
(defparameter in8-test (read-input-d8 #P"./inputs/08-test.txt"))

(defun d8-p1 (&optional (inp in8))
  (let ((n-unique 0))
    (iter (for (observations output) in inp)
      (iter (for obs in output)
	(when (member (fset:size obs) (list 2 3 4 7))
	  (incf n-unique))))
    n-unique))

;; (d8-p1 in8-test)
;; (d8-p1)

(defun demangle-display (observations)
  (let ((mapping (make-hash-table :test 'equal))
	(letter-counts (fset:empty-bag)))
    (iter (for obs in observations)
      (setq letter-counts (fset:bag-sum letter-counts obs))
      (case (fset:size obs)
	(2 (setf (gethash 1 mapping) obs))
	(3 (setf (gethash 7 mapping) obs))
	(4 (setf (gethash 4 mapping) obs))
	(7 (setf (gethash 8 mapping) obs))))
    (let ((bottom-left)
	  (bottom-right)
	  (top-left)
	  (top-right)
	  (center))
      (fset:do-bag-pairs (letter count letter-counts)
	(case count
	  (4 (setq bottom-left (fset:set letter)))
	  (9 (setq bottom-right (fset:set letter)))
	  (8 (when (fset:contains? (gethash 1 mapping) letter) (setq top-right (fset:set letter))))
	  (6 (setq top-left (fset:set letter)))))
      (setq center (fset:set-difference
		    (fset:set-difference (gethash 4 mapping)
					 (gethash 1 mapping))
		    top-left))
      (setf (gethash 0 mapping) (fset:set-difference (gethash 8 mapping) center))
      (setf (gethash 2 mapping) (fset:set-difference
				 (fset:set-difference (gethash 8 mapping) top-left)
				 bottom-right))
      (setf (gethash 3 mapping) (fset:set-difference
				 (fset:set-difference (gethash 8 mapping) top-left)
				 bottom-left))
      (setf (gethash 5 mapping) (fset:set-difference
				 (fset:set-difference (gethash 8 mapping) top-right)
				 bottom-left))
      (setf (gethash 6 mapping) (fset:set-difference (gethash 8 mapping) top-right))
      (setf (gethash 9 mapping) (fset:set-difference (gethash 8 mapping) bottom-left)))

    mapping))

(defun translate-display (obs num-to-segments)
  (iter (for i below 10)
    (when (fset:equal? obs (gethash i num-to-segments))
      (return i))))

(defun read-display (xs)
  (parse-integer (map 'string (lambda (x) (code-char (+ x (char-code #\0)))) xs)))

(defun d8-p2 (&optional (inp in8))
  (sum (mapcar (lambda (display)
		   (let ((num-to-segments (demangle-display (first display))))
		     (read-display
		      (iter (for obs in (second display))
			(collect (translate-display obs num-to-segments))))))
	       inp)))

;; (d8-p2 in8-test)
;; (d8-p2)

;; (def-suite day-08 :in all-tests)
;; (in-suite day-08)

;; (test test-name
;;   (is (= 1 0)))

;; f 9
;; a 8
;; c 8
;; d 7
;; g 7
;; b 6
;; e 4
