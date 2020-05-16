(in-package #:statecharts.dsl)




;;; macros and their helper that make up the statecharts DSL
(defun key-type-p (thing)
  (or (typep thing 'string)
      (typep thing 'symbol)
      (typep thing 'list)))




(defun parse-guard-clause (clause environment-symbol)
  (match clause
    ((guard (list (or 't 'otherwise) final-state)
	    (key-type-p final-state))
     `(make-instance 'sc.dsl::otherwise-clause :final-state ,final-state))
    ((guard  (list (or 'if-in-state 'when-in-state) in-state final-state)
    	     (and (key-type-p final-state)
    		  (key-type-p in-state)))
     `(make-instance 'sc.dsl::when-in-state-clause :final-state ,final-state
						   :in-state ,in-state))
    ((guard (list code final-state)
	    (key-type-p final-state))
     `(make-instance 'sc.dsl::guard-clause :final-state ,final-state
					   :fun #'(lambda (,environment-symbol)
						    (declare (ignorable ,environment-symbol))
						    ,code)
					   :code ',code))
    (otherwise (error "Could not parse guard clause: ~a" clause))))

(defun parse-final-state (final)
  (match final
    ((guard  (list (or 'if-in-state 'when-in-state) in-state final-state)
    	     (and (key-type-p final-state)
    		  (key-type-p in-state)))
     `(list (make-instance 'sc.dsl::when-in-state-clause :final-state ,final-state
    							 :in-state ,in-state)))
    ((cons (or 'cond 'guard) (cons (or (cons environment-symbol _) (list)) clauses))
     (if (not clauses)
	 (error "Couldn't parse final state specification: ~a" final))
     `(list ,@(mapcar #'(lambda (clause) (parse-guard-clause clause environment-symbol))
		      clauses)))
    ((or (type string) (type symbol) (type list))
     `(list (make-instance 'sc.dsl::transition-clause :final-state ,final)))
    (otherwise (error "Couldn't parse final state specification: ~a" final))))








(defparameter *environment* nil)
(defparameter *sub-state-of-or* nil)
(defparameter *nth-sub-state* -1)

(defun get-state-bit ()
  (if *sub-state-of-or* (ash 1 (incf *nth-sub-state*)) 0))


(defun get-sub-states-bits (e)
  (if (typep e 'sc.dsl::state)
      (+ (state-bit e) (sub-states-bits e))
      0))

;;;; helper macros
(defmacro %superstate (type name state-selector default-state description
		       entry exit reentry sub-states)
  (ecase type
    (cluster
     `(let* ((sub-states (let ((*sub-state-of-or* t)) (list ,@sub-states)))
	     (sub-states-bits (reduce #'logior sub-states :key #'get-sub-states-bits)))
	(if (not (find ,(alexandria:make-keyword default-state)
		       sub-states :key #'sc.dsl::name :test #'equal))
	    (error 'sc.cond::couldnt-find-default-state
		   :default-state ,default-state
		   :cluster ,name))
	(make-instance 'sc.dsl::cluster :name ,name :description ,description
					:on-entry ,entry
					:on-exit ,exit
					:on-reentry ,reentry
					:selector-type ',state-selector
					:default-state ,default-state
					:elements sub-states
					:sub-states-bits sub-states-bits
					:state-bit (get-state-bit))))
    (orthogonal
     `(let* ((sub-states (let ((*sub-state-of-or* nil)) (list ,@sub-states)))
	     (sub-states-bits (reduce #'logior sub-states :key #'get-sub-states-bits)))
	(make-instance 'sc.dsl::orthogonal :name ,name :description ,description
					   :on-entry ,entry
					   :on-reentry ,reentry
					   :on-exit ,exit
					   :elements sub-states
					   :sub-states-bits sub-states-bits
					   :state-bit (get-state-bit))))))

;;; statechart definition language

(defmacro -> (event initial final)
  "Given event with name EVENT, construct an object of type TRANSITION
from initial state with name INITIAL to final state with name
FINAL. If IF is non-nil, it is assumed to be a function of one
parameter, the statechart ENVIRONMENT and the transition will only
proceed if IF returns true.

==> TRANSITION"
  (let+ ((clauses (parse-final-state final)))
    `(sc.dsl::make-transition ,initial ,event ,clauses)))


(defmacro o (name (&key (description "") entry exit reentry) &body sub-states)
  "Returns an object of type ORTHOGONAL (with its sub-states being
active at the same time, i.e. a logical conjunction of the sub-states:
O(A1,...,AN) = A1 /\ A2 /\ ... /\ AN) with the name NAME that will
have SUB-STATES as its sub-states. The initial state will be choosen
by STATE-SELECTOR and DEFAULT-STATE. Valid symbols for STATE-SELECTOR
are D (for default) and H (for history). DESCRIPTION is an optional
explanation for the orthogonal cluster. If given the function ENTRY
will be called upon a state-change into the state NAME and EXIT will
be called upon a state-change out of the state NAME. EXIT and ENTRY a
functions of one variable and will be called with the ENVIRONMENT as
their parameter.

=> ORTHOGONAL"
   `(sc.dsl::%superstate orthogonal ,name nil nil ,description
			 ,entry ,exit ,reentry ,sub-states))

(defmacro c (name (state-selector default-state &key (description "")
						     entry exit reentry)
	     &body sub-states)
  "Returns an object of type CLUSTER (with only one sub-state being
active at all times, i.e. a logical conjunction of the sub-states:

O(A1,...,AN) = A1 + A2 + ... + AN) with the name NAME that will have
SUB-STATES as its sub-states. The initial state will be choosen by
STATE-SELECTOR and DEFAULT-STATE. Valid symbols for STATE-SELECTOR are
D (for default) and H (for history). DESCRIPTION is an optional
explanation for the orthogonal cluster. If given the function ENTRY
will be called upon a state-change into the state NAME and EXIT will
be called upon a state-change out of the state NAME. EXIT and ENTRY a
functions of one variable and will be called with the ENVIRONMENT as
their parameter.

=> CLUSTER"
  `(sc.dsl::%superstate cluster ,name ,state-selector ,default-state
			,description ,entry ,exit ,reentry ,sub-states))




(defmacro s (name &key (description "") entry exit reentry)
  "Returns an object of type STATE with the name NAME. DESCRIPTION is
an optional explanation for the orthogonal cluster. If given the
function ENTRY will be called upon a state-change into the state NAME
and EXIT will be called upon a state-change out of the state
NAME. EXIT and ENTRY a functions of one variable and will be called
with the ENVIRONMENT as their parameter.

=> STATE"
  `(make-instance 'sc.dsl::state
		  :name ,name :description ,description
		  :on-entry ,entry
		  :on-reentry ,reentry
		  :on-exit ,exit
		  :state-bit (get-state-bit)))


(defmacro act ((&optional (environment-symbol 'env)) &body code)
  `(make-instance 'sc.dsl::action
		  :code ',code
		  :fun #'(lambda (,environment-symbol)
			   (declare (ignorable ,environment-symbol))
			   ,@code)))


