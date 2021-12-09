(in-package #:advent-2021)

;; define function "pad-array" that takes two arguments, the second one is optional and defaults to 10
(defun pad-array (xs &optional (pad-val 10))
  "Pad 2D array with pad-val from all sides."
  ;; let binds variables for the computation inside it
  ;; equivalent to H = xs.height, W = xs.width, ...
  ;; let* is the same, but the variables H, W can be used already in the binding (in padded)
  (let* ((H (array-dimension xs 0))
	 (W (array-dimension xs 1))
	 (padded (make-array (list (+ H 2) (+ W 2)) :initial-element pad-val))) ; create a larger array filled with pad-val
    ;; here we have variables H, W, padded
    (iter (for row below H) 		; like for(row=0; row<H; row++)
      (iter (for col below W)
	; copy the original xs array values into the larger padded one.
	(setf (aref padded (1+ row) (1+ col))
	      (aref xs row col))))
    padded))

;; this can then be called as:
;; (pad-array some-array)
;; to pad the "some-array" with 10, or (pad-array some-array 24) to pad with 24
;; to call a function, you write (function arg1 arg2 ...), e.g. (+ 2 4) results in 6
;; (aref 2d-array 0 0) results in the top-left element of 2d-array, etc...

(defun lists-to-array (xs)
  "Convert list of lists into a 2D array (assuming all lists have the same length)."
  (let ((H (length xs))
	(W (length (first xs))))
    (make-array (list H W) :initial-contents xs)))

(defun read-input-d9 (path)
  "Parse day 9 input into a 2D array of numbers."
  (lists-to-array
   (mapcar 				; map each input line with the following lambda function
    (lambda (line)
      (mapcar 				; map each line character with the following lambda function
       (lambda (c)
	 ;; convert character to the number: ascii('3') - ascii('0') = 3
	 (- (char-int c) (char-int #\0)))
       (coerce line 'list) 		; convert from string to list of characters
       ))
    (uiop:read-file-lines path))))

(defparameter in9 (read-input-d9 #P"./inputs/09.txt"))
(defparameter in9-test (read-input-d9 #P"./inputs/09-test.txt"))

(defun local-minimum-p (xs row col)
  "True if the [row, col] coordinates are the minimum of their 4-neighborhood."
  (let ((query (aref xs row col))
	(top (aref xs (- row 1) col))
	(bottom (aref xs (+ row 1) col))
	(left (aref xs row (- col 1)))
	(right (aref xs row (+ col 1))))
    (< query (min top bottom left right)) ; true if the [row, col] value is smaller than the minimum of the neighbor values
    ))

(defun d9-p1 (&optional (inp in9))
  (let ((padded (pad-array inp))
	(H (array-dimension inp 0))
	(W (array-dimension inp 1))
	(risks 0))
    (iter (for row below H)
      (iter (for col below W)
	(when (local-minimum-p padded (1+ row) (1+ col))
	  (incf risks (1+ (aref padded (1+ row) (1+ col)))) ; increase risks by 1 + value of the minimum
	  )))
    risks))

;; (d9-p1 in9-test)
;; (d9-p1)

(defun get-smaller-neighbor (xs row col)
  "Returns coordinates of a smaller neighbor, or nil if none is smaller (at the basin bottom)."
  (let ((query (aref xs row col)) 	; height of the current [row, col] point
	(top (list (- row 1) col)) 	; coordinates of the neighbors
	(bottom (list (+ row 1) col))
	(left (list row (- col 1)))
	(right (list row (+ col 1))))
    (flet ((val (coords) (aref xs (first coords) (second coords)))) ; local function that extracts the value at coordinates

      (cond ; this cond would be equivalent to:
	((< (val top) query) top) 	; if (val(top) < query) {output top}
	((< (val bottom) query) bottom) ; elseif (val(bottom) < query) {output bottom}
	((< (val left) query) left) 	; elseif ...
	((< (val right) query) right) 	; elseif ...
	(t nil)) 			; elseif (True) {output nil}  (t / nil are lisp equivalent of True/False)
      )))

(defun flow-down (xs row col)
  "Find the bottom of the basin that current point [row, col] belongs to"
  (when (>= (aref xs row col) 9) ; height 9 belongs to no basin (represented as nil) -> immediately return nil
    (return-from flow-down nil))
  (let ((downhill (get-smaller-neighbor xs row col))) ; try to find smaller point in neighborhood
    (unless downhill (return-from flow-down (list row col))) ; if none is found, we are at the basin bottom -> return the current coordinates
    (flow-down xs (first downhill) (second downhill)) ; else return the coordinates that we would find if we started from the neighbor
    ;; let returns the result of the last form (...)
    )
  ;; flow-down function call will return the result of the last form (i.e. the value returned by the last (let ...))
  )

(defun d9-p2 (&optional (inp in9))
  (let ((padded (pad-array inp))
	(H (array-dimension inp 0))
	(W (array-dimension inp 1))
	(basins (fset:empty-bag) 	; bag: a data structure that remembers the counts of each thing we put inside
		))
    (iter (for row below H)
      (iter (for col below W)
	;; go through the height map and from each point flow down to the basin (or return special "nil" basin)
	(setq basins
	      (fset:with basins (flow-down padded (1+ row) (1+ col))) ; create a new bag with "basins" bag contents +the current point basin-bottom location
	      ) 			; and set: basins = the new bag
	))
    (let ((sizes)) ; sizes = empty list
      ;; go through all things in the bag
      (fset:do-bag-pairs (coords count basins)
	(when coords ;; skip the special nil basin (nil is basically boolean False)
	  (push count sizes) ;; add the basin size into the sizes list
	  ))
      ;; finally sort the basin sizes and multiply the three largest ones
      (setq sizes (sort sizes #'>))
      (* (first sizes) (second sizes) (third sizes)))))

;; (d9-p2 in9-test)
;; (d9-p2)

;; (def-suite day-09 :in all-tests)
;; (in-suite day-09)

;; (test test-name
;;   (is (= 1 0)))

