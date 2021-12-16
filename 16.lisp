(in-package #:advent-2021)

(defun padded-hex2bin (hex-char)
  (format nil "~4,'0b" (parse-integer
			(coerce (list hex-char) 'string)
			:radix 16)))

(defun parse-binary-int (str)
  (parse-integer str :radix 2))

(parseq:defrule bin-as-hex-char ()
    (char "0-9A-Z")
  (:function #'padded-hex2bin))

(parseq:defrule hex-BITS-transmission ()
    (+ bin-as-hex-char)
  (:lambda (&rest nibbles)
    (format nil "~{~a~}" nibbles)))

(defun BITS-transmission-hex-to-bin (hex-transmission)
  (parseq:parseq 'hex-bits-transmission hex-transmission))

(defstruct bits-packet
  version
  type
  size
  contents)

(parseq:defrule bits-bit ()
    (char "01")
  (:string))

(parseq:defrule bits-binary-number (n-bits)
    (rep n-bits bits-bit)
  (:string)
  (:function #'parse-binary-int))

(parseq:defrule bits-packet-version ()
    (rep 3 bits-bit)
  (:string)
  (:function #'parse-binary-int))

(parseq:defrule bits-packet-type-id ()
    (rep 3 bits-bit)
  (:string)
  (:external type-id)
  (:lambda (x)
    (let ((val (parse-binary-int x)))
      (setf type-id
	    (cond
	      ((= val 4) 'literal-value)
	      (t 'operator))))))

(parseq:defrule bits-packet ()
    (and bits-packet-header (or bits-literal-value bits-operator))
  (:let version type-id (packet-size 6))
  (:lambda (header contents)
    (make-bits-packet :version (first header) :type (second header) :size packet-size :contents contents)))

(parseq:defrule bits-packet-header ()
    (and bits-packet-version bits-packet-type-id))

(parseq:defrule bits-literal-value-part (initial-char)
    (and initial-char (rep 4 bits-bit))
  (:choose 1)
  (:string)
  (:external packet-size)
  (:lambda (x)
    (incf packet-size 5)
    x))

(parseq:defrule bits-literal-value ()
    (and (* (bits-literal-value-part "1"))
	 (bits-literal-value-part "0"))
  (:flatten)
  (:external type-id)
  (:lambda (&rest nibbles)
    (parse-binary-int (format nil "~{~a~}" nibbles)))
  (:test (x) (declare (ignore x)) (eq 'literal-value type-id)))

(parseq:defrule bits-operator ()
    (and bits-length-type-id bits-operator-length bits-operator-subpackets)
  (:let length-type operator-length)
  (:external type-id)
  (:not (&rest xs) (declare (ignore xs)) (eq 'literal-value type-id)))

(parseq:defrule bits-length-type-id ()
    bits-bit
  (:external length-type packet-size)
  (:lambda (x)
    (incf packet-size)
    (setf length-type
	  (if (string= x "0")
	      'bit-length
	      'n-subpackets))))

(parseq:defrule bits-operator-length-bits ()
    (rep 15 bits-bit)
  (:string)
  (:external length-type operator-length packet-size)
  (:test (x) (declare (ignore x))
    (eq length-type 'bit-length))
  (:lambda (str)
    (incf packet-size 15)
    (let ((val (parse-binary-int str)))
      (setf operator-length val)
      val)))

(parseq:defrule bits-operator-length-count ()
    (rep 11 bits-bit)
  (:string)
  (:external length-type operator-length packet-size)
  (:test (x) (declare (ignore x))
    (eq length-type 'n-subpackets))
  (:lambda (str)
    (incf packet-size 11)
    (setf operator-length (parse-binary-int str))))

(parseq:defrule bits-operator-length ()
    (or bits-operator-length-bits bits-operator-length-count))

(parseq:defrule bits-operator-subpackets ()
    (+ bits-packet)
  (:external length-type operator-length packet-size)
  (:test (&rest subpackets)
    (or
     (and (eq length-type 'bit-length)
	  (= operator-length (reduce #'+ (mapcar #'bits-packet-size subpackets)
				     :initial-value 0)))
     (and (eq length-type 'n-subpackets)
	  (= operator-length (length subpackets)))))
  (:lambda (&rest subpackets)
    (incf packet-size (reduce #'+ (mapcar #'bits-packet-size subpackets) :initial-value 0))
    subpackets))

(parseq:defrule bits-n-bits-subpackets ()
    (rep operator-length bits-bit)
  (:string)
  (:external length-type operator-length)
  (:test (x) (declare (ignore x)) (eq 'bit-length length-type)))

(parseq:defrule bits-n-subpackets ()
    (rep operator-length bits-packet)
  (:external length-type operator-length)
  (:test (x) (declare (ignore x)) (eq 'n-subpackets length-type)))

(defun bits-parse-transmission (transmission)
  (parseq:parseq 'bits-packet (bits-transmission-hex-to-bin transmission) :junk-allowed t))

(defun read-input-d16 (path)
  (bits-parse-transmission (uiop:stripln (uiop:read-file-string path))))

(defparameter in16 (read-input-d16 #P"./inputs/16.txt"))
(defparameter in16-test (read-input-d16 #P"./inputs/16-test.txt"))

(defun d16-p1 (&optional (inp in16))

  )

;; (d16-p1 in16-test)
;; (d16-p1)

(defun d16-p2 (&optional (inp in16))
  )

;; (d16-p2 in16-test)
;; (d16-p2)

(def-suite day-16 :in all-tests)
(in-suite day-16)

(test d16-hex-decode
  (is (string= "110100101111111000101000"
	       (bits-transmission-hex-to-bin "D2FE28")))
  (is (string= "00111000000000000110111101000101001010010001001000000000"
	       (bits-transmission-hex-to-bin "38006F45291200"))))

(test d16-p1-example
  (is (= (d16-p1 in16-test) 42)))

(test d16-p2-example
      (is (= (d16-p2 in16-test) 42)))
