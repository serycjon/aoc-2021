(in-package #:advent-2021)

(defstruct cuboid
  min-x max-x
  min-y max-y
  min-z max-z)

(defmethod empty-p ((obj cuboid))
  (or (empty-p (axis-interval obj 'x))
      (empty-p (axis-interval obj 'y))
      (empty-p (axis-interval obj 'z))))

(defstruct interval min max)

(defmethod empty-p ((obj interval))
  (< (interval-max obj) (interval-min obj)))

(defmethod intersect-p ((a interval) (b interval))
  (<= (max (interval-min a) (interval-min b))
      (min (interval-max a) (interval-max b))))

(defmethod intersect ((a interval) (b interval))
  (make-interval :min (max (interval-min a) (interval-min b))
		 :max (min (interval-max a) (interval-max b))))

(defmethod intersect ((a cuboid) (b cuboid))
  (let ((xs (intersect (axis-interval a 'x) (axis-interval b 'x)))
	(ys (intersect (axis-interval a 'y) (axis-interval b 'y)))
	(zs (intersect (axis-interval a 'z) (axis-interval b 'z))))
    (make-cuboid :min-x (interval-min xs) :max-x (interval-max xs)
		 :min-y (interval-min ys) :max-y (interval-max ys)
		 :min-z (interval-min zs) :max-z (interval-max zs))))

(defmethod axis-interval ((obj cuboid) axis)
  (ecase axis
    (x (make-interval :min (cuboid-min-x obj) :max (cuboid-max-x obj)))
    (y (make-interval :min (cuboid-min-y obj) :max (cuboid-max-y obj)))
    (z (make-interval :min (cuboid-min-z obj) :max (cuboid-max-z obj)))))

(defmethod intersect-p ((a cuboid) (b cuboid))
  (and (intersect-p (axis-interval a 'x) (axis-interval b 'x))
       (intersect-p (axis-interval a 'y) (axis-interval b 'y))
       (intersect-p (axis-interval a 'z) (axis-interval b 'z))))

(defgeneric cut (a b &key &allow-other-keys))

(defmethod cut ((a interval) (b interval) &key &allow-other-keys)
  (remove-if #'empty-p
	     (list (make-interval :min (interval-min a) :max (- (interval-min b) 1))
		   (make-interval :min (max (interval-min b) (interval-min a))
				  :max (min (interval-max b) (interval-max a)))
		   (make-interval :min (+ (interval-max b) 1) :max (interval-max a)))))

(defmethod with-replaced-axis ((obj cuboid) axis (new-interval interval))
  (let ((xs (if (eq 'x axis) new-interval (axis-interval obj 'x)))
	(ys (if (eq 'y axis) new-interval (axis-interval obj 'y)))
	(zs (if (eq 'z axis) new-interval (axis-interval obj 'z))))
    (make-cuboid :min-x (interval-min xs) :max-x (interval-max xs)
		 :min-y (interval-min ys) :max-y (interval-max ys)
		 :min-z (interval-min zs) :max-z (interval-max zs))))

(defparameter *axes* '(x y z))

(defmethod cut ((a cuboid) (b cuboid) &key (axes *axes*))
  (if (or (null axes) (not (intersect-p a b)))
      (list a)
      (let* ((cur-axis-intervals (cut (axis-interval a (car axes)) (axis-interval b (car axes))))
	     (cur-cuboids (mapcar (lambda (interval) (with-replaced-axis a (car axes) interval)) cur-axis-intervals)))
	(apply #'append
	       (mapcar (lambda (cub) (cut cub b :axes (cdr axes))) cur-cuboids)))))

(defun read-input-d22 (path)
  (mapcar
   (lambda (line)
     (ppcre:register-groups-bind (((lambda (x) (if (string= x "on") 'on 'off)) on)
				  (#'parse-integer min-x)
				  (#'parse-integer max-x)
				  (#'parse-integer min-y)
				  (#'parse-integer max-y)
				  (#'parse-integer min-z)
				  (#'parse-integer max-z))
	 ("(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)" line)
       (list on (make-cuboid :min-x min-x :max-x max-x
			     :min-y min-y :max-y max-y
			     :min-z min-z :max-z max-z))))
   (uiop:read-file-lines path)))
(defparameter in22 (read-input-d22 #P"./inputs/22.txt"))
(defparameter in22-test (read-input-d22 #P"./inputs/22-test.txt"))
(defparameter in22-test2 (read-input-d22 #P"./inputs/22-test2.txt"))

(defun intersect-any-p (a bs)
  (fset:do-set (b bs)
    (when (intersect-p a b)
      (return-from intersect-any-p t)))
  nil)

(defun turn-on (cuboid reactor)
  (let ((sub-cuboids (fset:set cuboid))
	(new-reactor reactor))
    (fset:do-set (reactor-cub reactor)
      (let ((new (fset:empty-set)))
	(fset:do-set (sub-cub sub-cuboids)
	  (iter (for new-sub in (cut sub-cub reactor-cub))
	    (fset:adjoinf new new-sub)))
	(setq sub-cuboids new)))
    (fset:do-set (sub-cub sub-cuboids)
      (unless (intersect-any-p sub-cub reactor)
	(fset:adjoinf new-reactor sub-cub)))
    new-reactor))

(defun turn-off (cuboid reactor)
  (let ((new-reactor (fset:empty-set)))
    (fset:do-set (reactor-cub reactor)
      (iter (for part in (cut reactor-cub cuboid))
	(unless (intersect-p part cuboid)
	  (fset:adjoinf new-reactor part))))
    new-reactor))

(defmethod volume ((obj cuboid))
  (* (1+ (- (cuboid-max-x obj) (cuboid-min-x obj)))
     (1+ (- (cuboid-max-y obj) (cuboid-min-y obj)))
     (1+ (- (cuboid-max-z obj) (cuboid-min-z obj)))))

(defun count-on (reactor)
  (let ((sum 0))
    (fset:do-set (cub reactor)
      (incf sum (volume cub)))
    sum))

(defun d22-p1 (&optional (inp in22))
  (let ((on-cuboids (fset:empty-set))
	(limits (make-cuboid :min-x -50 :max-x 50
			     :min-y -50 :max-y 50
			     :min-z -50 :max-z 50)))
    (iter
      (for (operation cuboid) in inp)
      (for limited = (intersect cuboid limits))
      (unless (empty-p limited)
       (setq on-cuboids
	     (ecase operation
	       (on (turn-on limited on-cuboids))
	       (off (turn-off limited on-cuboids))))))
    (count-on on-cuboids)))

;; (d22-p1 in22-test)
;; (d22-p1)

(defun d22-p2 (&optional (inp in22))
  (let ((on-cuboids (fset:empty-set)))
    (iter
      (for (operation cuboid) in inp)
      (setq on-cuboids
	    (ecase operation
	      (on (turn-on cuboid on-cuboids))
	      (off (turn-off cuboid on-cuboids)))))
    (count-on on-cuboids)))

;; (d22-p2 in22-test)
;; (d22-p2)

(def-suite day-22 :in all-tests)
(in-suite day-22)

(test d22-p1-example
  (is (= 39 (d22-p1 in22-test))))

(test d22-p2-example
  (is (= 2758514936282235 (d22-p2 in22-test2))))
