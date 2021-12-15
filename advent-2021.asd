;;;; ..asd

(asdf:defsystem #:advent-2021
  :description "Describe . here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:iterate #:fiveam #:parseq #:fset #:alexandria #:cl-ppcre #:queues.priority-queue)
  :components #.(append '((:file "package"))
			(loop for day from 1 to 15
			      collect (list :file (format nil "~2,'0D" day)))))
