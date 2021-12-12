(in-package #:advent-2021)

(defun read-input-d12 (path)
  (let ((graph (make-hash-table :test 'equal)))
    (iter (for line in (uiop:read-file-lines path))
      (destructuring-bind (a b) (uiop:split-string (string-trim " " line)
						   :separator '(#\-))
	(push a (gethash b graph))
	(push b (gethash a graph))))
    graph))

(defparameter in12 (read-input-d12 #P"./inputs/12.txt"))
(defparameter in12-test (read-input-d12 #P"./inputs/12-test.txt"))

(defun big-cave-p (cave-name)
  (string= cave-name (string-upcase cave-name)))

(defun d12-p1 (&optional (graph in12))
  (let ((stack `(("start" ,(fset:set "start"))))
	(n-paths 0))
    (iter (for (cave visited) = (pop stack))
      (when (null cave) (return))
      ;; (format t "~a; ~a~%" cave visited)
      (when (equal cave "end")
	(incf n-paths))
      (iter (for neighbor in (gethash cave graph))
	(when (or (big-cave-p neighbor)
		  (not (fset:contains? visited neighbor)))
	  (push (list neighbor (fset:with visited neighbor)) stack))))
    n-paths))

;; (d12-p1 in12-test)
;; (d12-p1)

(defun usable-twice-p (cave-name)
  (and (not (big-cave-p cave-name))
       (not (string= cave-name "start"))
       (not (string= cave-name "end"))))

(defun d12-p2 (&optional (graph in12))
  (let ((stack `(("start" ,(fset:set "start") nil)))
	(n-paths 0))
    (iter (for (cave visited used-small-twice) = (pop stack))
      (when (null cave) (return))
      (when (equal cave "end")
	(incf n-paths))
      (iter (for neighbor in (gethash cave graph))
	(let* ((been-there (fset:contains? visited neighbor))
	       (second-visit (and (usable-twice-p neighbor)
				  been-there
				  (not used-small-twice))))
	  (when (or (big-cave-p neighbor)
		    (not been-there)
		    second-visit)
	    (push (list neighbor (fset:with visited neighbor)
			(or used-small-twice second-visit))
		  stack)))))
    n-paths))

;; (d12-p2 in12-test)
;; (d12-p2)

(def-suite day-12 :in all-tests)
(in-suite day-12)

(test d12-p1-example
  (is (= (d12-p1 in12-test) 10)))

(test d12-p2-example
  (is (= (d12-p2 in12-test) 36)))

