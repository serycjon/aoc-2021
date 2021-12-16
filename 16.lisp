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

(defun packet-type (type-id)
  (case type-id
    (0 'sum)
    (1 'product)
    (2 'minimum)
    (3 'maximum)
    (4 'literal-value)
    (5 'greater-than)
    (6 'less-than)
    (7 'equal-to)
    (t 'operator)))

(defun bits-type-to-lisp (type)
  (case type
    (sum '+)
    (product '*)
    (minimum 'min)
    (maximum 'max)
    (greater-than 'bits->)
    (less-than 'bits-<)
    (equal-to 'bits-=)))

(defun bits-> (a b)
  (if (> a b) 1 0))

(defun bits-< (a b)
  (if (< a b) 1 0))

(defun bits-= (a b)
  (if (= a b) 1 0))

(defun length-type (length-type-id)
  (if (string= length-type-id "0")
      'n-bits
      'n-packets))

(defun parse-literal-value (in-str)
  (iter
    (with str = in-str)
    (with literal-parts = nil)
    (for group-i from 1)
    (let ((last-group (string= (subseq str 0 1) "0"))
	  (data (subseq str 1 5)))
      (push data literal-parts)
      (setq str (subseq str 5))
      (when last-group
	(return
	  (values
	   (parse-binary-int (format nil "~{~a~}" (nreverse literal-parts)))
	   (* 5 group-i)))))))

(defun parse-operator-length (str)
  (let ((type (length-type (subseq str 0 1))))
    (if (eq type 'n-bits)
	(values type (parse-binary-int (subseq str 1 16)) 16)
	(values type (parse-binary-int (subseq str 1 12)) 12))))

(defun parse-packet (str)
  (let ((version (parse-binary-int (subseq str 0 3)))
	(type (packet-type (parse-binary-int (subseq str 3 6))))
	(packet-size 6)			; 6-bit header
	)
    (if (eq type 'literal-value)
	(multiple-value-bind (value n-bits) (parse-literal-value (subseq str 6))
	  (incf packet-size n-bits)
	  (make-bits-packet :version version :type type :size packet-size :contents value))
	;; operators:
	(multiple-value-bind (operator-length-type operator-length n-bits) (parse-operator-length (subseq str 6))
	  (incf packet-size n-bits)
	  (let* ((the-rest (subseq str packet-size))
		 (contents)
		 (contents-length 0))
	    (iter (for i from 1)
	      (let* ((subpacket (parse-packet the-rest))
		     (n-bits (bits-packet-size subpacket)))
		(push subpacket contents)
		(incf contents-length n-bits)
		(setq the-rest (subseq the-rest n-bits)))
	      (when (or (and (eq operator-length-type 'n-packets)
			     (= i operator-length))
			(and (eq operator-length-type 'n-bits)
			     (= contents-length operator-length)))
		(return)))
	    (incf packet-size contents-length)
	    (make-bits-packet :version version :type type :size packet-size :contents (nreverse contents)))))))

(defun bits-parse-transmission (transmission)
  (parse-packet (bits-transmission-hex-to-bin transmission)))

(defun read-input-d16 (path)
  (bits-parse-transmission (uiop:stripln (uiop:read-file-string path))))

(defparameter in16 (read-input-d16 #P"./inputs/16.txt"))
(defparameter in16-test (read-input-d16 #P"./inputs/16-test.txt"))

(defun count-version-sum (packet)
  (let ((cur (bits-packet-version packet)))
    (if (eq 'literal-value (bits-packet-type packet))
	cur
	(+ cur (reduce #'+ (mapcar #'count-version-sum (bits-packet-contents packet))
		       :initial-value 0)))))

(defun d16-p1 (&optional (inp in16))
  (count-version-sum inp))

;; (d16-p1 in16-test)
;; (d16-p1)

(defun packet-to-lisp (packet)
  (let ((type (bits-packet-type packet)))
    (if (eq 'literal-value type)
	(bits-packet-contents packet)
	(append (list (bits-type-to-lisp type))
		(mapcar #'packet-to-lisp (bits-packet-contents packet))))))

(defun d16-p2 (&optional (inp in16))
  (eval (packet-to-lisp inp)))

;; (d16-p2 in16-test)
;; (d16-p2)

(def-suite day-16 :in all-tests)
(in-suite day-16)

(test d16-hex-decode
  (is (string= "110100101111111000101000"
	       (bits-transmission-hex-to-bin "D2FE28")))
  (is (string= "00111000000000000110111101000101001010010001001000000000"
	       (bits-transmission-hex-to-bin "38006F45291200"))))

(test d16-p1-examples
  (is (= 16 (count-version-sum (bits-parse-transmission "8A004A801A8002F478"))))
  (is (= 12 (count-version-sum (bits-parse-transmission "620080001611562C8802118E34"))))
  (is (= 23 (count-version-sum (bits-parse-transmission "C0015000016115A2E0802F182340"))))
  (is (= 31 (count-version-sum (bits-parse-transmission "A0016C880162017C3686B18A3D4780")))))

(test d16-p2-examples
  (is (= 3 (eval (packet-to-lisp (bits-parse-transmission "C200B40A82")))))
  (is (= 54 (eval (packet-to-lisp (bits-parse-transmission "04005AC33890")))))
  (is (= 7 (eval (packet-to-lisp (bits-parse-transmission "880086C3E88112")))))
  (is (= 9 (eval (packet-to-lisp (bits-parse-transmission "CE00C43D881120")))))
  (is (= 1 (eval (packet-to-lisp (bits-parse-transmission "D8005AC2A8F0")))))
  (is (= 0 (eval (packet-to-lisp (bits-parse-transmission "F600BC2D8F")))))
  (is (= 0 (eval (packet-to-lisp (bits-parse-transmission "9C005AC2F8F0")))))
  (is (= 1 (eval (packet-to-lisp (bits-parse-transmission "9C0141080250320F1802104A08"))))))
