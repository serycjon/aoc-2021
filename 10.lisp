(in-package #:advent-2021)

(parseq:defrule d10-chunk-boundary ()
    (char "(){}[]<>"))

(parseq:defrule d10-line ()
    (+ d10-chunk-boundary))

(defun parse-d10-line (line)
  (parseq:parseq 'd10-line line))

(defun read-input-d10 (path)
  (mapcar
   #'parse-d10-line
   (uiop:read-file-lines path)))

(defparameter in10 (read-input-d10 #P"./inputs/10.txt"))
(defparameter in10-test (read-input-d10 #P"./inputs/10-test.txt"))

(define-condition unbalanced-chunk-error (error)
  ((message :initarg :message)
   (expected :initarg :expected)
   (real :initarg :real)))

(defmethod print-object ((object unbalanced-chunk-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (message expected real) object
     (format stream "~a. Expected ~a, got ~a instead."
	     message expected real))))

(defun unbalanced-chunk-error (message expected real)
  (error 'unbalanced-chunk-error :message message :expected expected :real real))

(define-condition incomplete-input-error (error)
  ((stack-left :initarg :stack-left)))

(defun incomplete-input-error (message stack)
  (error 'incomplete-input-error :message message :stack-left stack))

(defun closing-char (opening-c)
  (cond
    ((char= opening-c #\() #\))
    ((char= opening-c #\<) #\>)
    ((char= opening-c #\{) #\})
    ((char= opening-c #\[) #\])
    (t (error "Unknown opening character"))))

(defun closes-p (closing-c opening-c)
  (char= closing-c (closing-char opening-c)))

(defun check-line-syntax (line)
  (iter
    (with stack = nil)
    (for c in line)
    (cond
      ((member c '(#\( #\{ #\[ #\<)) (push c stack))
      ((closes-p c (first stack)) (pop stack))
      (t (unbalanced-chunk-error "Unbalanced chunk" (closing-char (first stack)) c)))
    (finally (unless (null stack) (incomplete-input-error "Incomplete line" stack)))))

(defun syntax-error-score (c)
  (cond
    ((char= c #\)) 3)
    ((char= c #\]) 57)
    ((char= c #\}) 1197)
    ((char= c #\>) 25137)
    (t (error "Unknown closing character"))))

(defun d10-p1 (&optional (inp in10))
  (iter
    (with error-score = 0)
    (for line in inp)
    (handler-case (check-line-syntax line)
      (incomplete-input-error (c) (declare (ignore c)))
      (unbalanced-chunk-error (c)
	(with-slots (real) c
	  (incf error-score (syntax-error-score real)))))
    (finally (return error-score))))

;; (d10-p1 in10-test)
;; (d10-p1)

(defun autocomplete-score (stack-left)
  (iter
    (with score = 0)
    (for c in stack-left)
    (setq score (+ (* 5 score)
		   (cond
		     ((char= c #\() 1)
		     ((char= c #\[) 2)
		     ((char= c #\{) 3)
		     ((char= c #\<) 4)
		     (t (error "Unknown chunk opening char")))))
    (finally (return score))))

(defun final-autocomplete-score (scores)
  (let ((sorted (sort scores #'<)))
    (nth (floor (length sorted) 2) sorted)))

(defun d10-p2 (&optional (inp in10))
  (iter
    (with error-scores = '())
    (for line in inp)
    (handler-case (check-line-syntax line)
      (incomplete-input-error (c)
	(with-slots (stack-left) c
	  (push (autocomplete-score stack-left) error-scores)))
      (unbalanced-chunk-error (c) (declare (ignore c))))
    (finally (return (final-autocomplete-score error-scores)))))

;; (d10-p2 in10-test)
;; (d10-p2)

(def-suite day-10 :in all-tests)
(in-suite day-10)

(test example-p1
  (is (= (d10-p1 in10-test) 26397)))

(test example-p2
  (is (= (d10-p2 in10-test) 288957)))
