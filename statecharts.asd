;;;; statecharts.asd

(asdf:defsystem #:statecharts
  :description "Lisp implementation of statecharts"
  :author "R. Klaitter <klawitterrenee@gmail.com>"
  :version "0.0.1"
  :license "Apache 2.0"
  :depends-on (#:iterate
		#:alexandria
		#:let-plus
		#:group-by
		#:trivia
		#:trivia.quasiquote
		#:parachute)
  :serial t
  :components ((:file "package")
	       (:file "api")
	       (:file "dsl-objects")
	       (:file "state-name")
	       (:file "chart-objects")
	       (:file "chart-compute")
	       (:file "fsm-objects")
	       (:file "dsl-definition")
	       (:file "fsm-runtime")
	       (:file "uml-output")))

