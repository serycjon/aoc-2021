(in-package #:advent-2021)

(parseq:defrule d4-number ()
    (+ digit)
  (:string)
  (:function #'parse-integer))

(parseq:defrule d4-draw ()
    (+ (and d4-number (? ",")))
  (:lambda (&rest values) (mapcar #'first values)))

(parseq:defrule bingo-line ()
    (+ (and (* " ") d4-number))
  (:lambda (&rest values) (mapcar #'second values)))

(parseq:defrule bingo-board ()
    (rep 5 (and bingo-line #\Newline))
  (:lambda (&rest values) (mapcar #'first values)))

(parseq:defrule d4-input ()
    (and d4-draw #\Newline #\Newline
	 (+ (and bingo-board (? #\Newline))))
  (:choose 0 3)
  (:lambda (draw boards)
    (list draw
	  (mapcar #'first boards))))

(defun read-input-d4 (path)
  (parseq:parseq 'd4-input (uiop:read-file-string path)))

(defparameter in4 (read-input-d4 #P"./inputs/04.txt"))

(defparameter in4-test (read-input-d4 #P"./inputs/04-test.txt"))

(defun is-winning (board draw)
  (let ((winning-row (iter (for row below 5)
		       (when (every #'identity
				    (iter (for col below 5)
				      (collecting (member (nth col (nth row board)) draw))))
			 (return row))))
	(winning-col (iter (for col below 5)
		       (when (every #'identity
				    (iter (for row below 5)
				      (collecting (member (nth col (nth row board)) draw))))
			 (return col)))))
    (when winning-row (return-from is-winning (list 'row winning-row)))
    (when winning-col (return-from is-winning (list 'col winning-col)))))

(defun board-score (board draw)
  (let ((score 0))
    (iter (for row in board)
      (iter (for x in row)
	(unless (member x draw)
	  (incf score x))))
    score))

(defun d4-p1 (&optional (inp in4))
  (destructuring-bind (draw boards) inp
    (let ((drawn-so-far))
      (iter (for x in draw)
	(push x drawn-so-far)
	(iter (for board in boards)
	  (when (is-winning board drawn-so-far)
	    (return-from d4-p1 (* x (board-score board drawn-so-far)))))))))
;; (d4-p1 in4-test)
;; (d4-p1)

(defun d4-p2 (&optional (inp in4))
  (destructuring-bind (draw boards) inp
    (let ((drawn-so-far)
	  (used-boards)
	  (last-score))
      (iter (for x in draw)
	(push x drawn-so-far)
	(iter
	  (for board in boards)
	  (for board-i below (length boards))
	  (when (and (not (member board-i used-boards)) (is-winning board drawn-so-far))
	    (setq last-score (* x (board-score board drawn-so-far)))
	    (push board-i used-boards))))
      last-score)))

;; (d4-p2 in4-test)
;; (d4-p2)

(def-suite day-04 :in all-tests)
(in-suite day-04)

(test winning-row
  (is (equal
       (list 'row 2)
       (is-winning (first (second in4-test))
		   '(14 9 21 7 0 3 16)))))
(test winning-col
  (is (equal
       (list 'col 3)
       (is-winning (first (second in4-test))
		   '(14 15 18 0 16 4 11)))))
(test not-winning
  (is (not
       (is-winning (first (second in4-test))
		   '(14 15 18 0)))))

