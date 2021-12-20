(in-package #:advent-2021)

(parseq:defrule d20 ()
    (and image-enhancement-algorithm #\Newline d20-image)
  (:choose 0 2))

(parseq:defrule image-enhancement-algorithm ()
    (and d20-pixels
	 #\Newline)
  (:choose 0)
  (:lambda (&rest xs)
    (make-array (length xs) :initial-contents xs)))

(parseq:defrule d20-pixels ()
    (+ (char "#."))
  (:lambda (&rest xs)
    (mapcar (lambda (c)
	      (if (char= c #\#) 1 0))
	    xs)))

(defun d20-parse-map (&rest listmap)
  (let ((map (fset:empty-set))
	(known (fset:empty-set)))
    (iter
      (for r from 0)
      (for row in listmap)
      (iter
	(for c from 0)
	(for x in row)
	(fset:adjoinf known (complex r c))
	(when (= x 1)
	  (fset:adjoinf map (complex r c)))))
    (list map known)))

(parseq:defrule d20-image ()
    (+ (thing-followed-by
	d20-pixels
	#\Newline))
  (:function #'d20-parse-map))

(defun read-input-d20 (path)
  (parseq:parseq 'd20 (uiop:read-file-string path)))

(defparameter in20 (read-input-d20 #P"./inputs/20.txt"))
(defparameter in20-test (read-input-d20 #P"./inputs/20-test.txt"))

(defun map-interesting-pixels (map &optional (sz 2))
  (let ((interesting (fset:empty-set)))
    (fset:do-set (coord map)
      (iter (for dr from (- sz) to sz)
	(iter (for dc from (- sz) to sz)
	  (fset:adjoinf interesting (+ coord (complex dr dc))))))
    interesting))

(defun sample-map-patch (map coord known-coords value-at-infinity)
  (parse-integer
   (with-output-to-string (stream)
     (iter (for dr from -1 to 1)
       (iter (for dc from -1 to 1)
	 (let ((query (+ coord (complex dr dc))))
	   (write-char
	    (if (= 1 (if (fset:lookup known-coords query)
			 (if (fset:lookup map query) 1 0)
			 value-at-infinity))
		#\1 #\0)
	    stream)))))
   :radix 2))

(defun step-gol (rules map known-coords value-at-infinity)
  (let ((interesting (map-interesting-pixels map))
	(new-map (fset:empty-set)))
    (fset:do-set (coord interesting)
      (when (= 1 (svref rules (sample-map-patch map coord known-coords value-at-infinity)))
	(fset:adjoinf new-map coord)))
    ;; return new-map updated known-coords, updated value-at-infinity
    (values new-map (fset:union known-coords interesting) (if (= 0 value-at-infinity)
							      (svref rules 0)
							      (svref rules (- (expt 2 9) 1))))))

(defun draw-map (coord-set)
  (let ((coords (fset:convert 'list coord-set)))
   (destructuring-bind (H W x-shift y-shift)
       (iter (for x in coords)
	 ;; find the result dimensions
	 (maximize (imagpart x) into W)
	 (maximize (realpart x) into H)
	 (minimize (imagpart x) into x-shift)
	 (minimize (realpart x) into y-shift)
	 (finally (return (list (- H y-shift) (- W x-shift) x-shift y-shift))))
     (let ((grid (make-array (list (1+ H) (1+ W)) :initial-element #\Space)))
       (iter (for coord in coords)
	 ;; mark the coords with unicode FULL_BLOCK
	 (setf (aref grid (- (realpart coord) y-shift) (- (imagpart coord) x-shift)) #\FULL_BLOCK))

       ;; convert to list of strings
       (format nil
	       "~%~{~a~%~}"
	       (iter (for row below (array-dimension grid 0))
		 (collecting
		     (coerce
		      (iter (for col below (array-dimension grid 1))
			(collect (aref grid row col)))
		      'string))))))))

(defun d20-p1 (&optional (inp in20))
  (destructuring-bind (rules (init-map init-known-coords)) inp
    (let ((map init-map)
	  (known-coords init-known-coords)
	  (value-at-inf 0))
      (iter (for i below 2)
	(multiple-value-bind (new-map new-known-coords new-value-at-inf)
	    (step-gol rules map known-coords value-at-inf)
	  (setq map new-map
		known-coords new-known-coords
		value-at-inf new-value-at-inf)))
      (values (fset:size map) value-at-inf))))

;; (d20-p1 in20-test)
;; (d20-p1)

(defun d20-p2 (&optional (inp in20))
  (destructuring-bind (rules (init-map init-known-coords)) inp
    (let ((map init-map)
	  (known-coords init-known-coords)
	  (value-at-inf 0))
      (iter (for i below 50)
	(multiple-value-bind (new-map new-known-coords new-value-at-inf)
	    (step-gol rules map known-coords value-at-inf)
	  (setq map new-map
		known-coords new-known-coords
		value-at-inf new-value-at-inf)))
      (values (fset:size map) value-at-inf))))

;; (d20-p2 in20-test)
;; (d20-p2)

(def-suite day-20 :in all-tests)
(in-suite day-20)

(test d20-p1-example
  (is (= 34 (sample-map-patch (first (second in20-test)) (complex 2 2) (second (second in20-test)) 0)))
  (is (= 35 (d20-p1 in20-test))))

;; (test d20-p2-example
;;   (is (= 3351 (d20-p2 in20-test))))
