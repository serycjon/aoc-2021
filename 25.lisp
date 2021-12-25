(in-package #:advent-2021)

(defun list-to-2d-array (lists)
  (make-array (list (length lists) (length (first lists)))
	      :initial-contents lists))

(defun read-input-d25 (path)
  (list-to-2d-array
   (mapcar
    (lambda (line)
      (mapcar (lambda (c)
		(ecase c
		  (#\> '>)
		  (#\v 'v)
		  (#\. nil)))
	      (coerce line 'list)))
    (uiop:read-file-lines path))))

(defparameter in25 (read-input-d25 #P"./inputs/25.txt"))
(defparameter in25-test (read-input-d25 #P"./inputs/25-test.txt"))

(defun draw-cucumbers (map)
  (format nil "~%~{~a~%~}"
	  (iter (for row below (array-dimension map 0))
	    (collecting (coerce
			 (iter (for col below (array-dimension map 1))
			   (collecting (if (aref map row col)
					   (ecase (aref map row col)
					     (> #\>) (v #\v))
					   #\.)))
			 'string)))))

(defun move-cucumber (map row col direction)
  (let ((next-row (mod (if (eq direction 'v) (1+ row) row)
		       (array-dimension map 0)))
	(next-col (mod (if (eq direction '>) (1+ col) col)
		       (array-dimension map 1))))
    (unless (and (eq (aref map row col) direction)
		 (null (aref map next-row next-col)))
      (error "not empty!"))
    (rotatef (aref map row col)
	     (aref map next-row next-col))
    ))

(defun cucumber-can-move (map row col direction)
  (let ((next-row (mod (if (eq direction 'v) (1+ row) row)
		       (array-dimension map 0)))
	(next-col (mod (if (eq direction '>) (1+ col) col)
		       (array-dimension map 1))))
    (and (eq (aref map row col) direction)
	 (null (aref map next-row next-col)))))

(defun move-cucumber (map row col direction)
  (let ((next-row (mod (if (eq direction 'v) (1+ row) row)
		       (array-dimension map 0)))
	(next-col (mod (if (eq direction '>) (1+ col) col)
		       (array-dimension map 1))))
    (when (and (eq (aref map row col) direction)
	       (null (aref map next-row next-col)))
      (rotatef (aref map row col)
	       (aref map next-row next-col))
      t)))

(defun step-cucumbers (map)
  (let ((stopped t))
    (let ((coords (iter outer (for row below (array-dimension map 0))
		    (iter (for col below (array-dimension map 1))
		      (when (cucumber-can-move map row col '>)
			(in outer (collect (list row col))))))))
      (when coords (setq stopped nil))
      (iter (for (row col) in coords)
	(move-cucumber map row col '>)))
    (let ((coords (iter outer (for row below (array-dimension map 0))
		    (iter (for col below (array-dimension map 1))
		      (when (cucumber-can-move map row col 'v)
			(in outer (collect (list row col))))))))
      (when coords (setq stopped nil))
      (iter (for (row col) in coords)
	(move-cucumber map row col 'v)))

    stopped))

(defun d25-p1 (&optional (inp in25))
  (let ((map (alexandria:copy-array inp)))
    (iter (for i from 1)
      (when (step-cucumbers map)
	(return i)))))

(def-suite day-25 :in all-tests)
(in-suite day-25)

(test d25-p1-example
  (is (= 58 (d25-p1 in25-test))))
