(in-package #:statecharts)



;;; macros and their helper that make up the statecharts DSL
(defun parse-guard-clause (clause environment-symbol)
  (match clause
    ((guard (list (or 't 'otherwise) final-state)
	    (or (typep final-state 'string)
		(typep final-state 'symbol)))
     `(make-instance 'statecharts.dsl::otherwise-clause :final-state ,final-state))
    ((guard (list code final-state)
	    (or (typep final-state 'string)
		(typep final-state 'symbol)))
     `(make-instance 'statecharts.dsl::guard-clause :final-state ,final-state
						    :fun #'(lambda (,environment-symbol)
							     (ignorable ,environment-symbol)
							     ,code)
						    :code ',code))
    (otherwise (error "Could not parse guard clause: ~a" clause))))



(defun parse-final-state (final)
  (match final
    ((or (type string) (type symbol))
     `(list (make-instance 'statecharts.dsl::transition-clause :final-state ,final)))
    ((cons (or 'cond 'guard) (cons (or (cons environment-symbol _) (list)) clauses))
     (if (not clauses)
	 (error "Couldn't parse final state specification: ~a" final))
     `(list ,@(mapcar #'(lambda (clause) (parse-guard-clause clause environment-symbol)) clauses)))
    (otherwise (error "Couldn't parse final state specification: ~a" final))))




(defun %check-defstatechart-arguments (name description definitions)
  (declare (ignore definitions))
  (cond
    ((not (symbolp name))
     (error 'invalid-chart-syntax
	    :message "Name needs to be a symbol."
	    :offending-code name))
    ((not (stringp description))
     (error 'invalid-chart-syntax
	    :message "Description needs to be a string."
	    :offending-code description))))


;;;; helper macros
(defmacro %superstate (type name state-selector default-state description entry exit reentry sub-states)
  (ecase type
    (cluster
     `(let ((sub-states (list ,@sub-states)))
	(if (not (find ,default-state sub-states :key #'name :test #'string=))
	    (error 'couldnt-find-default-state
		   :default-state ,default-state
		   :cluster ,name))
	(make-instance 'statecharts.dsl::cluster :name ,name :description ,description
						 :on-entry ,entry
						 :on-exit ,exit
						 :on-reentry ,reentry
						 :selector-type ',state-selector
						 :default-state ,default-state
						 :elements sub-states)))
    (orthogonal
     `(make-instance 'statecharts.dsl::orthogonal :name ,name :description ,description
						  :on-entry ,entry
						  :on-reentry ,reentry
						  :on-exit ,exit
						  :elements (list ,@sub-states)))))

;;; statechart definition language

(defmacro -> (event initial final)
  "Given event with name EVENT, construct an object of type TRANSITION
from initial state with name INITIAL to final state with name
FINAL. If IF is non-nil, it is assumed to be a function of one
parameter, the statechart ENVIRONMENT and the transition will only
proceed if IF returns true.

==> TRANSITION"
  (let+ ((clauses (parse-final-state final)))
    `(statecharts.dsl::make-transition ,initial ,event ,clauses)))


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
  `(sc::%superstate orthogonal ,name nil nil
		    ,description ,entry ,exit ,reentry ,sub-states))

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
  `(sc::%superstate cluster ,name ,state-selector ,default-state
		    ,description ,entry ,exit ,reentry ,sub-states))




(defmacro s (name &key (description "") entry exit reentry)
  "Returns an object of type STATE with the name NAME. DESCRIPTION is
an optional explanation for the orthogonal cluster. If given the
function ENTRY will be called upon a state-change into the state NAME
and EXIT will be called upon a state-change out of the state
NAME. EXIT and ENTRY a functions of one variable and will be called
with the ENVIRONMENT as their parameter.

=> STATE"
  `(make-instance 'statecharts.dsl::state
		  :name ,name :description ,description
		  :on-entry ,entry
		  :on-reentry ,reentry
		  :on-exit ,exit))


(defmacro act ((&optional (environment-symbol 'env)) &body code)
  `(make-instance 'statecharts.dsl::action
		  :code ',code
		  :fun #'(lambda (,environment-symbol)
			   (declare (ignorable ,environment-symbol))
			   ,@code)))


(defun %create-state-chart (name root description)
  (let* ((states (chart::compute-substates root))
	 (transitions (chart::compute-transitions root '() root))
	 (fsm-states (fsm::create-states states))
	 (events (remove-duplicates (mapcar #'event-name transitions)))
	 (default-state (first (remove-if-not #'chart::is-default-state states))))
    (set-transitions-for-fsm/states states fsm-states transitions)
    (make-instance 'statecharts::statechart
		   :name (string name)
		   :description description
		   :root root
		   :states states
		   :fsm/states fsm-states
		   :transitions transitions
		   :events events
		   :default-state default-state)))


(defmacro defstatechart ((name &key (description ""))
			 &body definitions)
  (%check-defstatechart-arguments name description definitions)
  (dsl::clear-id)
  `(defparameter ,name (%create-state-chart ',name
					    (progn ,@definitions)
					    ,description)))




