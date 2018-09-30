(in-package #:statecharts)

;;; macros and their helper that make up the statecharts DSL


(defun %t (initial-name event final-name if)
  (labels ((format-name (name)
	     (cond
	       ((stringp name) name)
	       ((listp name) (format nil "~{~a~^::~}" name))
	       (t (error "This should happen.")))))
    (make-instance 'transition
		   :name (format nil "~a -> ~a"
				 (format-name initial-name)
				 (format-name final-name))
		   :event event
		   :initial-state initial-name
		   :final-state final-name
		   :guards (if if if (constantly t)))))


(defun %s (name description entry exit)
  (make-instance 'state :name name :description description
			:on-entry entry
			:on-exit exit))



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


(defmacro %superstate (type name state-selector default-state description entry exit sub-states)
  (case type
    (cluster
     `(let ((sub-states (list ,@sub-states)))
	(if (not (find ,default-state sub-states :key #'name :test #'string=))
	    (error 'couldnt-find-default-state
		   :default-state ,default-state
		   :cluster ,name))
	(make-instance 'cluster :name ,name :description ,description
				:on-entry ,entry
				:on-exit ,exit
				:selector-type ',state-selector
				:default-state ,default-state
				:elements sub-states)))
    (orthogonal
     `(make-instance 'orthogonal :name ,name :description ,description
				 :on-entry ,entry
				 :on-exit ,exit
				 :elements (list ,@sub-states)))))

;;; statechart definition language

(defmacro -> (event initial final &key when-in-state)
  "Given event with name EVENT, construct an object of type TRANSITION
from initial state with name INITIAL to final state with name
FINAL. If IF is non-nil, it is assumed to be a function of one
parameter, the statechart ENVIRONMENT and the transition will only
proceed if IF returns true.

==> TRANSITION"
  `(statecharts::%t ,initial ,event ,final ,when-in-state))


(defmacro o (name (&key (description "") entry exit)
	     &body sub-states)
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
  `(%superstate orthogonal ,name nil nil
		,description ,entry ,exit ,sub-states))

(defmacro c (name (state-selector default-state &key (description "") entry exit)
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
  `(%superstate cluster ,name ,state-selector ,default-state
     ,description ,entry ,exit ,sub-states))


(defmacro s (name &key (description "") entry exit)
  "Returns an object of type STATE with the name NAME. DESCRIPTION is
an optional explanation for the orthogonal cluster. If given the
function ENTRY will be called upon a state-change into the state NAME
and EXIT will be called upon a state-change out of the state
NAME. EXIT and ENTRY a functions of one variable and will be called
with the ENVIRONMENT as their parameter.

=> STATE"
  `(statecharts::%s ,name ,description ,entry ,exit))


(defmacro :or (&rest guards))

(defmacro :and (&rest guards))

(defmacro act ((&optional (environment-symbol 'env)) &body code)
  `(make-instance 'sc:action
		  :code ',code
		  :fun #'(lambda (,environment-symbol)
			   (declare (ignorable ,environment-symbol))
			   ,@code)))


(defun %create-state-chart (name root description)
  (let* ((states (compute-substates root))
	 (fsm-states (create-fsm-states states))
	 (transitions (compute-transitions root '() root))
	 (events (remove-duplicates (mapcar #'event-name transitions)))
	 (default-state (first (remove-if-not #'is-default-state states))))
    (set-transitions-for-fsm-states states fsm-states transitions)
    (make-instance 'statecharts::statechart
		   :name (string name)
		   :description description
		   :root root
		   :states states
		   :fsm-states fsm-states
		   :transitions transitions
		   :events events
		   :default-state default-state)))


(defmacro defstatechart ((name &key (description ""))
			 &body definitions)
  (%check-defstatechart-arguments name description definitions)
  (clear-id)
  `(defparameter ,name (%create-state-chart ',name
					    (progn ,@definitions)
					    ,description)))










;; (in-readtable :fare-quasiquote)



;; (defun parse (list)
;;   (match ))



;; (defun dsl (&rest statechart-description)
;;   "Parse statecharts DSL ")


