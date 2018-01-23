(in-package #:statecharts)

;;; 



(defun %t (initial-key event final-key if)
  (labels ((format-key (key)
	     (cond
	       ((stringp key) key)
	       ((listp key) (format nil "~{~a~^::~}" key))
	       (t (error "This should happen.")))))
    (make-instance 'transition
		   :name (format nil "~a -> ~a"
				 (format-key initial-key)
				 (format-key final-key))
		   :event event
		   :initial-state initial-key
		   :final-state final-key
		   :guard (if if if (constantly t)))))


(defun %s (name description entry exit)
  (make-instance 'state :name name :description description
			:on-entry (if entry entry (constantly t))
			:on-exit (if exit exit (constantly t))))



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
  `(make-instance ',type :name ,name :description ,description
			 :on-entry (if ,entry ,entry (constantly t))
			 :on-exit (if ,exit ,exit (constantly t))
			 :selector-type ',state-selector
			 :default-state ,default-state
			 :elements (list ,@sub-states)))

;;; statechart definition language

(defmacro -> (event initial final &key if)
  "Given event with name EVENT, construct an object of type TRANSITION
from initial state with name INITIAL to final state with name
FINAL. If IF is non-nil, it is assumed to be a function of one
parameter, the statechart ENVIRONMENT and the transition will only
proceed if IF returns true.

==> TRANSITION"
  `(statecharts::%t ,initial ,event ,final ,if))


(defmacro o (name (state-selector default-state &key (description "") entry exit)
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
  `(%superstate orthogonal ,name ,state-selector ,default-state
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


(defmacro defstatechart ((name &key (description "")) &body definitions)
  (%check-defstatechart-arguments name description definitions)
  `(let* ((statechart (make-instance 'statecharts::statechart :name ,(string name)
							      :description ,description)))
     (setf (root statechart) (progn ,@definitions)
	   (states statechart) (compute-substates (root statechart))
	   ;;(transitions statechart) (compute-transitions (root statechart) '())
	   )
     (defparameter ,name statechart)
     statechart))









