;;;; statecharts.asd

(asdf:defsystem #:statecharts
  :description "Lisp implementation of statecharts"
  :author "R. Klawitter <klawitterrenee@gmail.com>"
  :version "0.0.1"
  :license "Apache 2.0"
  :depends-on (#:iterate
		#:alexandria
		#:let-plus
		#:group-by
		#:trivia
		#:trivia.quasiquote
		#:parachute
		#:queues.simple-queue
		#:fare-quasiquote-extras)
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "api")
	       (:file "state-name-objects")
	       (:file "dsl-objects")	       
	       (:file "chart-objects")
	       (:file "fsm-objects")
	       (:file "state-name-compute")
	       (:file "chart-compute")
	       (:file "dsl-definition")
	       (:file "fsm-compute")
	       (:file "fsm-runtime")
	       (:file "uml-output")))

