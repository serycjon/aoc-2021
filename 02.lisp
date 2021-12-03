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
   (ecase dir
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
   (ecase dir
     (down (incf aim dist))
     (up (decf aim dist))
     (forward
      (incf x dist)
      (incf depth (* aim dist))))
   (finally (return (* x depth)))))

;; (d2-p1)
;; (d2-p2)

;; interesting parser from reddit
;; https://www.reddit.com/r/adventofcode/comments/r6zd93/2021_day_2_solutions/hmwc2nu/
;; (defun parse (stream)
;;   (iterate (for line :in-stream stream :using #'read-line)
;;            (ppcre:register-groups-bind ((#'ensure-keyword command) (#'parse-integer n))
;;                ("(\\w+) (\\d+)" line)
;; 	       (collect (cons command n)))))


;; another interesting approach:
;; https://www.reddit.com/r/adventofcode/comments/r6zd93/2021_day_2_solutions/hmx50fj/
;; (ql:quickload :parseq)

;; (parseq:defrule up () "up" (:constant '(0 -1)))
;; (parseq:defrule down () "down" (:constant '(0 1)))
;; (parseq:defrule forward () "forward" (:constant '(1 0)))
;; (parseq:defrule value () (rep (0 10) digit) (:string) (:function #'parse-integer))
;; (parseq:defrule move () (and (or up down forward) " " value) (:lambda (m i s) (mapcar (lambda (v) (* v s)) m)))

;; (print (reduce #'* (reduce (lambda (x y) (mapcar #'+ x y))
;;                            (mapcar (lambda (line) (parseq:parseq 'move line))
;;                                    (uiop:read-file-lines "02.input")))))

;; (print (let ((x 0) (y 0) (aim 0))
;;          (mapc (lambda (line)
;;                  (let ((movement (parseq:parseq 'move line)))
;;                    (setf aim (+ aim (cadr movement)))
;;                    (setf x (+ x (car movement)))
;;                    (setf y (+ y (* aim (car movement))))))
;;                (uiop:read-file-lines "02.input"))
;;          (* x y)))
