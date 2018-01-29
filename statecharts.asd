;;;; statecharts.asd

(asdf:defsystem #:statecharts
  :description "Describe statecharts here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iterate
		#:alexandria
		#:let-plus
		#:group-by
		#:parachute
		#:cl-dot)
  :serial t
  :components ((:file "package")
	       (:file "api")
	       (:file "dsl-objects")
	       (:file "state-name")
	       (:file "fsm-objects")
	       (:file "fsm-compute")
	       (:file "dsl-definition")
	       (:file "fsm-runtime")))

