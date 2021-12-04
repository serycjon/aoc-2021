;;;; ..asd

(asdf:defsystem #:advent-2021
  :description "Describe . here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:iterate #:fiveam #:parseq)
  :components ((:file "package")
               (:file "01")
	       (:file "02")
	       (:file "03")
	       (:file "04")))
