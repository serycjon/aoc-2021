(in-package #:advent-2021)

(defun parse-snailfish-num (str)
  (snailfish-from-list (read-from-string
    (substitute #\Space #\, 
		(substitute #\) #\]
			    (substitute #\( #\[ str))))))

(defclass snailfish-tree ()
  ((val :initarg :value :initform nil :accessor value)
   (left :initarg :left :initform nil :accessor left)
   (right :initarg :right :initform nil :accessor right)
   (parent :initarg :parent :initform nil :accessor parent)
   (left-child :initform nil :initarg :left-child :accessor left-child-p)))

(defmethod print-object ((obj snailfish-tree) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (val left right) obj
      (if val
	(format stream "~a" val)
	(format stream "<~a, ~a> left: ~a"
		(when (value left) (value left))
		(when (value right) (value right))
		(left-child-p obj))))))

(defmethod leaf-p ((obj snailfish-tree))
  (value obj))

(defmethod root-p ((obj snailfish-tree))
  (null (parent obj)))

(defun snailfish-from-list (xs &optional left-child)
  (if (numberp xs)
      (make-instance 'snailfish-tree :value xs :left-child left-child)
      (let ((node (make-instance 'snailfish-tree
				 :left (snailfish-from-list (first xs) t)
				 :right (snailfish-from-list (second xs) nil)
				 :left-child left-child)))
	(unless (numberp (left node))
	  (setf (parent (left node)) node))
	(unless (numberp (right node))
	  (setf (parent (right node)) node))
	node)))

(defmethod to-list ((obj snailfish-tree))
  (if (leaf-p obj)
      (value obj)
      (list (to-list (left obj))
	    (to-list (right obj)))))

(defun read-input-d18 (path)
  (mapcar #'parse-snailfish-num (uiop:read-file-lines path)))

(defparameter in18 (read-input-d18 #P"./inputs/18.txt"))
(defparameter in18-test (read-input-d18 #P"./inputs/18-test.txt"))

(defun add-snailfish-nums (a b)
  ;; (format t "--------------~%")
  (let ((node (make-instance 'snailfish-tree
			     :left a
			     :right b)))
    (setf (parent (left node)) node)
    (setf (left-child-p (left node)) t)
    (setf (parent (right node)) node)
    (snailfish-reduce node)))

(defun traverse (tree mode &optional (depth 0))
  (when (leaf-p tree)
    (when (and (eq mode 'split) (>= (value tree) 10))
      ;; (format t "split ~a~%" tree)
      (split tree)
      (return-from traverse t))
    (return-from traverse nil))
  (when (and (eq mode 'explode) (>= depth 4))
    ;; (format t "explode ~a~%" tree)
    (explode tree)
    (return-from traverse t))
  (when (traverse (left tree) mode (1+ depth))
    (return-from traverse t))
  (when (traverse (right tree) mode (1+ depth))
    (return-from traverse t)))

(defun snailfish-reduce (x)
  (iter
    ;; (format t "~a~%" (to-list x))
    (unless (or
	     (traverse x 'explode)
	     (traverse x 'split))
      (return)))
  ;; (format t "~a~%" (to-list x))
  x)

(defmethod explode ((obj snailfish-tree))
  ;; add-to-left
  (let ((cur obj))
    (iter
      ;; (format t "up: ~a~%" (to-list cur))
      (when (or (root-p cur) (not (left-child-p cur)))
	(return))
      (setq cur (parent cur)))
    (unless (root-p cur)
      (setq cur (left (parent cur)))
      (iter
	(when (null cur) (return))
	(when (leaf-p cur)
	  (incf (value cur) (value (left obj))))
	(setq cur (right cur)))))

  ;; add-to-right
  (let ((cur obj))
    (iter
      (when (or (root-p cur) (left-child-p cur))
	(return))
      (setq cur (parent cur)))
    (unless (root-p cur)
      (setq cur (right (parent cur)))
      (iter
	(when (null cur) (return))
	(when (leaf-p cur)
	  (incf (value cur) (value (right obj))))
	(setq cur (left cur)))))

  ;; replace current
  (setf (value obj) 0)
  (setf (left obj) nil)
  (setf (right obj) nil))

(defmethod split ((obj snailfish-tree))
  (let* ((val (value obj))
	 (left-val (floor val 2))
	 (right-val (ceiling val 2)))
    (setf (value obj) nil)
    (setf (left obj) (make-instance 'snailfish-tree :value left-val :left-child t :parent obj))
    (setf (right obj) (make-instance 'snailfish-tree :value right-val :left-child nil :parent obj)))
  t)

(defmethod magnitude ((obj snailfish-tree))
  (if (leaf-p obj)
      (value obj)
      (+ (* 3 (magnitude (left obj)))
	 (* 2 (magnitude (right obj))))))

(defun d18-p1 (&optional (inp in18))
  (let* ((nums (mapcar #'snailfish-from-list (mapcar #'to-list inp))) ;lazy copy
	 (result (first nums)))
    (iter (for num in (rest nums))
      (setq result (add-snailfish-nums result num))
      ;; (format t "~A~%" (to-list result))
      )
    (magnitude result)))

;; (d18-p1 in18-test)
;; (d18-p1)

(defun d18-p2 (&optional (inp in18))
  ;; lazy copy
  (let ((nums (mapcar #'to-list inp))
	(max-sum 0))
    (iter (for i below (length nums))
      (iter (for j from (1+ i) below (length nums))
	(setq max-sum (max max-sum
			   (magnitude (add-snailfish-nums
				       (snailfish-from-list (nth i nums))
				       (snailfish-from-list (nth j nums))))
			   (magnitude (add-snailfish-nums
				       (snailfish-from-list (nth j nums))
				       (snailfish-from-list (nth i nums))))))
	;; (format t "~a, ~a~%" i j)
	))
    max-sum))

;; (d18-p2 in18-test)
;; (d18-p2)

(def-suite day-18 :in all-tests)
(in-suite day-18)

(test d18-p1-example
  (is (equal (to-list (parse-snailfish-num "[[[[1,1],[2,2]],[3,3]],[4,4]]"))
	     (to-list (add-snailfish-nums
		       (add-snailfish-nums
			(add-snailfish-nums
			 (parse-snailfish-num "[1,1]")
			 (parse-snailfish-num "[2,2]"))
			(parse-snailfish-num "[3,3]"))
		       (parse-snailfish-num "[4,4]")))))
  (is (equal (to-list (parse-snailfish-num "[[[[3,0],[5,3]],[4,4]],[5,5]]"))
	     (to-list (add-snailfish-nums
		       (parse-snailfish-num "[[[[1,1],[2,2]],[3,3]],[4,4]]")
		       (parse-snailfish-num "[5,5]")))))
  (is (equal (to-list (parse-snailfish-num "[[[[3,0],[5,3]],[4,4]],[5,5]]"))
	     (to-list (add-snailfish-nums
		       (add-snailfish-nums
			(add-snailfish-nums
			 (add-snailfish-nums
			  (parse-snailfish-num "[1,1]")
			  (parse-snailfish-num "[2,2]"))
			 (parse-snailfish-num "[3,3]"))
			(parse-snailfish-num "[4,4]"))
		       (parse-snailfish-num "[5,5]")))))
  (is (equal (to-list (parse-snailfish-num "[[[[5,0],[7,4]],[5,5]],[6,6]]"))
	     (to-list (add-snailfish-nums
		       (parse-snailfish-num "[[[[3,0],[5,3]],[4,4]],[5,5]]")
		       (parse-snailfish-num "[6,6]")))))
  ;; slightly larger
  (is (equal (to-list (parse-snailfish-num "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"))
	     (to-list (add-snailfish-nums
		       (parse-snailfish-num "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")
		       (parse-snailfish-num "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"))))))

(to-list (add-snailfish-nums
		       (parse-snailfish-num "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")
		       (parse-snailfish-num "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")))
(test d18-p2-example
  (is (= 3993 (d18-p2 in18-test))))
