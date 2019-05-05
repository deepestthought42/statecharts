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
  :components ((:file "utils")
	       (:file "conditions")
	       (:file "state-name-objects")
	       (:file "dsl-objects")	       
	       (:file "chart-objects")
	       (:file "fsm-objects")
	       (:file "interface-objects")
	       (:file "state-name-compute")
	       (:file "chart-compute")
	       (:file "dsl-definition")
	       (:file "fsm-compute")
	       (:file "fsm-runtime")
	       (:file "interface")
	       (:file "uml-output")))

