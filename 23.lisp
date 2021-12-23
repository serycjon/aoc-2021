(in-package #:advent-2021)

(defun read-input-d23 (path)
  (uiop:read-file-lines path)
  (uiop:read-file-string path))

(defparameter in23 (read-input-d23 #P"./inputs/23.txt"))
(defparameter in23-test (read-input-d23 #P"./inputs/23-test.txt"))

(defun d23-p1 ()
  ;; solved by hand...
  15160)

;; (d23-p1 in23-test)
;; (d23-p1)

(defun draw-state (state)
  (flet ((draw-amphipod (x) (if x
				(ecase x
				  (a #\a)
				  (b #\b)
				  (c #\c)
				  (d #\d))
				#\.)))
    (let ((layout '((0 1 nil 2 nil 3 nil 4 nil 5 6)
		    (nil nil 7 nil 11 nil 15 nil 19 nil nil)
		    (nil nil 8 nil 12 nil 16 nil 20 nil nil)
		    (nil nil 9 nil 13 nil 17 nil 21 nil nil)
		    (nil nil 10 nil 14 nil 18 nil 22 nil nil))))
      (format nil "~%~{~A~%~}"
       (iter (for line in layout)
	 (collect (coerce
		   (iter (for i in line)
		     (collect (cond ((null i) #\Space)
				    (t (draw-amphipod (aref state i))))))
		   'string)))))))

(defun can-go-home (amphipod state j)
  "check that it goes to the right home, that there are no different-kind amphipods there and that it goes to the bottom.
Also returns number of steps needed inside the home"
  (let ((home-start (case amphipod (a 7) (b 11) (c 15) (d 19)))
	(home-end (case amphipod (a 10) (b 14) (c 18) (d 22))))
    (values
     (and
      ;; the right home
      (<= home-start j home-end)

      ;; no different-kind amphipod there
      (iter (for i from home-start to home-end)
	(always (let ((other (aref state i)))
		  (or (null other)
		      (eq amphipod other)))))

      ;; at the bottom
      (iter (for i from (1+ j) to home-end)
	(never (null (aref state i)))))
     (1+ (- j home-start)))))

(defun can-leave-home (pseudo-amphipod state i)
  "check that it is at home top.  Also returns number of steps to get out of home"
  (let ((home-start (ecase pseudo-amphipod (ha 7) (hb 11) (hc 15) (hd 19))))
    (values
     (iter (for k from home-start to (- i 1))
       (always (null (aref state k))))
     (1+ (- i home-start)))))

(defparameter *pseudo-path-map* '(0 1 ha 2 hb 3 hc 4 hd 5 6))

(defun pseudo-path (i j)
  (iter
    (with reverse-pseudo-path = nil)
    (with started = nil)
    (for x in *pseudo-path-map*)
    (when (or (eq x i)
	      (eq x j))
      (when started
	(push x reverse-pseudo-path))
      (setq started (not started)))
    (when started
      (push x reverse-pseudo-path))
    (finally (return (if (eq (car reverse-pseudo-path) i) reverse-pseudo-path (nreverse reverse-pseudo-path))))))

(defun pseudo-path-free (state pseudo-path)
  ;; ignore the first position
  (iter (for x in pseudo-path)
    (always (or (symbolp x)
		(null (aref state x))))))

(defun i-to-pseudo (i)
  (cond
    ((< i 7) i)
    ((<= 7 i 10) 'ha)
    ((<= 11 i 14) 'hb)
    ((<= 15 i 18) 'hc)
    ((<= 19 i 22) 'hd)))

(defun can-move (state i j)
  (let ((amphipod (aref state i))
	(pseudo-path nil)
	(pseudo-i (i-to-pseudo i))
	(pseudo-j (i-to-pseudo j))
	(cost 0))
    (when amphipod
      (let ((possible
	      (if (< i 7)
		  (and (multiple-value-bind (possible home-cost) (can-go-home amphipod state j)
			 (setq cost home-cost)
			 possible)
		       (setq pseudo-path (pseudo-path pseudo-i pseudo-j))
		       (pseudo-path-free state (cdr pseudo-path)))
		  (and (< j 7)
		       (multiple-value-bind (possible home-cost) (can-leave-home pseudo-i state i)
			 (setq cost home-cost)
			 possible)
		       (setq pseudo-path (pseudo-path pseudo-i pseudo-j))
		       (pseudo-path-free state (cdr pseudo-path))))))
	(when possible
	  ;; (format t "path: ~a; home: ~a~%" pseudo-path cost)
	  (* (+ cost (length pseudo-path) -1)
	     (ecase amphipod (a 1) (b 10) (c 100) (d 1000))))))))

(defparameter *init-state* (make-array 23 :initial-contents '(nil nil nil nil nil nil nil d d d b c c b c a b a d b a c a)))
(defparameter *test-state* (make-array 23 :initial-contents '(b d c c nil nil nil nil nil a a b c b c d b a a nil nil d d)))

(defun all-moves (state)
  "return list of (i j cost) lists"
  (iter outer (for i below 23)
      (iter (for j below 23)
	(let ((cost (can-move state i j)))
	  (when cost
	    (in outer (collect (list i j cost))))))))

(defun apply-move (state move)
  (let ((new-state (alexandria:copy-array state)))
    (rotatef (aref new-state (first move)) (aref new-state (second move)))
    new-state))

(defun amphipods-home-p (state)
  (and
   (iter (for i from 7 to 10)
     (always (eq 'a (aref state i))))
   (iter (for i from 11 to 14)
     (always (eq 'b (aref state i))))
   (iter (for i from 15 to 18)
     (always (eq 'c (aref state i))))
   (iter (for i from 19 to 22)
     (always (eq 'd (aref state i))))))

(defun optimize-amphipods (init-state)
  (let ((queue (priority-queue:make-pqueue #'<))
	(closed (fset:empty-set)))
    ;; init
    (priority-queue:pqueue-push (list init-state nil) 0 queue)
    (iter
      (for ((state path) state-cost) = (multiple-value-list (priority-queue:pqueue-pop queue)))
      (when (amphipods-home-p state) (return (values state-cost (nreverse (cons state path)))))
      (unless (fset:contains? closed state)
	(iter (for (i j move-cost) in (all-moves state))
	  (priority-queue:pqueue-push
	   (list (apply-move state (list i j))
		 (cons state path))
	   (+ state-cost move-cost)
	   queue))
	(fset:adjoinf closed state)))))

(defun d23-p2 ()
  (let ((state (make-array 23 :initial-contents '(nil nil nil nil nil nil nil d d d b c c b c a b a d b a c a))))
    (multiple-value-bind (total-cost path) (optimize-amphipods state)
      (iter (for state in path)
	(format t "~a~%" (draw-state state)))
      total-cost)))

;; (d23-p2 in23-test)
;; (d23-p2)

(def-suite day-23 :in all-tests)
(in-suite day-23)

;; (test d23-p1-example
;;   (is (= (d23-p1 in23-test) 42)))

;; (test d23-p2-example
;;   (is (= (d23-p2 in23-test) 42)))
