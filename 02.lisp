(in-package #:advent-2021)

(defun parse-line-d2 (line)
  (mapcar #'read-from-string (uiop:split-string line)))

(defun read-input-d2 (path)
  (mapcar #'parse-line-d2 (uiop:read-file-lines path)))

(defparameter in2 (read-input-d2 #P"./inputs/02.txt"))

(defun d2-p1 ()
 (iter
   (with x = 0)
   (with depth = 0)
   (for (dir dist) in in2)
   (case dir
     (forward (incf x dist))
     (down (incf depth dist))
     (up (decf depth dist)))
   (finally (return (* x depth)))))

(defun d2-p2 ()
 (iter
   (with x = 0)
   (with depth = 0)
   (with aim = 0)
   (for (dir dist) in in2)
   (case dir
     (down (incf aim dist))
     (up (decf aim dist))
     (forward
      (incf x dist)
      (incf depth (* aim dist))))
   (finally (return (* x depth)))))

;; (d2-p1)
;; (d2-p2)
