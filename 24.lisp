(in-package #:advent-2021)

(defun last-param (line)
  (parse-integer (car (last (uiop:split-string line)))))

(defun list-to-array (xs)
  (make-array (length xs) :initial-contents xs))

(defun read-input-d24 (path)
  (list-to-array
   (let ((lines (uiop:read-file-lines path))
	 (block-len 18))
     (iter (for i below 14)
       (let ((z-div-line (nth (+ (* i block-len) 4) lines))
	     (x-add-line (nth (+ (* i block-len) 5) lines))
	     (y-add-line (nth (+ (* i block-len) 15) lines)))
	 (collect (mapcar #'last-param (list z-div-line x-add-line y-add-line))))))))

(defparameter in24 (read-input-d24 #P"./inputs/24.txt"))

(defun neql (a b)
  (if (= a b) 0 1))

(defun monad-fun (level-rules input z)
  (destructuring-bind (z-div x-add y-add) level-rules
    (let ((x (neql input (+ x-add (mod z 26)))))
      (+ (* (floor z z-div) (+ (* 25 x) 1)) (* x (+ input y-add))))))

(defun solve-monad-max (lvls &optional (z 0) (level 0))
  (if (>= level (length lvls))
      (return-from solve-monad-max (when (= z 0) (list 'x))))

  (destructuring-bind (z-div x-add y-add) (aref lvls level)
    (declare (ignorable y-add))
    (if (= 26 z-div)
	(let* ((inp (+ x-add (mod z 26)))
	       (new-z (monad-fun (aref lvls level) inp z)))
	  (return-from solve-monad-max
	    (when (<= 1 inp 9)
	      (let ((from-below (solve-monad-max lvls new-z (1+ level))))
		(when from-below (cons inp from-below))))))
	(iter (for inp from 9 downto 1)
	  (let* ((new-z (monad-fun (aref lvls level) inp z))
		 (from-below (solve-monad-max lvls new-z (1+ level))))
	    (when from-below (return-from solve-monad-max (cons inp from-below)))))))
  nil)

(defun d24-p1 (&optional (inp in24))
  (format nil "~{~a~}" (butlast (solve-monad-max inp))))

(defun solve-monad-min (lvls &optional (z 0) (level 0))
  (if (>= level (length lvls))
      (return-from solve-monad-min (when (= z 0) (list 'x))))

  (destructuring-bind (z-div x-add y-add) (aref lvls level)
    (declare (ignorable y-add))
    (if (= 26 z-div)
	(let* ((inp (+ x-add (mod z 26)))
	       (new-z (monad-fun (aref lvls level) inp z)))
	  (return-from solve-monad-min
	    (when (<= 1 inp 9)
	      (let ((from-below (solve-monad-min lvls new-z (1+ level))))
		(when from-below (cons inp from-below))))))
	(iter (for inp from 1 to 9)
	  (let* ((new-z (monad-fun (aref lvls level) inp z))
		 (from-below (solve-monad-min lvls new-z (1+ level))))
	    (when from-below (return-from solve-monad-min (cons inp from-below)))))))
  nil)

(defun d24-p2 (&optional (inp in24))
  (format nil "~{~a~}" (butlast (solve-monad-min inp))))
