;;;; statecharts.asd

(asdf:defsystem #:statecharts
  :description "Describe statecharts here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iterate
		#:alexandria
		#:let-plus
		#:group-by)
  :serial t
  :components ((:file "package")
	       (:file "api")
               (:file "statecharts")
	       (:file "compose")
	       (:file "chart-definition")))

