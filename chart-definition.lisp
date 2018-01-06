(in-package #:statecharts)

;;; 


(defun %register-event (statechart event-name transition-object)
  (let+ (((&slots events) statechart)
	 (event (alexandria:if-let (existing (gethash event-name events))
		  existing
		  (setf (gethash event-name events)
			(make-instance 'event :name event-name)))))
    (push transition-object (registered-transitions event))))


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


(defun %s (name description entry exit selector)
  (make-instance 'state :name name :description description
			:on-entry (if entry entry (constantly t))
			:selector selector
			:on-exit (if exit exit (constantly t))))



(defun %dereference-key (superstate key)
  (labels ((valid-descriptor (desc)
	     (unless (stringp desc)
	       (error 'invalid-state-descriptor
		      :descriptor desc))))
    (cond
      ;; (/ "A" "B" "C") references with respect to the root of the tree:
      ;; -> "C" within "B" within "A"
      ((and (listp key) (equal '/ (first key))
	    (mapcar #'valid-descriptor (cdr key)))
       (cdr key))
      ;; ("A" "B") references within the current super state:
      ;; "B" within "A" within the current superstate
      ((and (listp key)
	    (mapcar #'valid-descriptor key)
	    (or (equal nil superstate)
		(mapcar #'valid-descriptor superstate)))
       (append superstate key))
      ;; "A" references "A" within the current superstate
      ((stringp key)
       (append superstate (list key)))
      (t (error 'invalid-state-descriptor
		:descriptor key)))))


(defun %register-state (statechart superstate state-name state-object)
  (let+ (((&slots states) statechart)
	 (key (%dereference-key superstate state-name))
	 (existing (gethash key states)))
    (if existing (error 'state-exists :key key))
    (setf (gethash key states) state-object)
    state-object))


(defun %register-transition (statechart initial-key final-key transition-object)
  (let+ (((&slots transitions) statechart)
	 (key (append initial-key final-key))
	 (existing (gethash key transitions)))
    (if existing (error 'transition-exists :key key))
    (setf (gethash key transitions) transition-object))
  transition-object)



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


(defmacro with-new-superstate (name &body body)
  `(let ((superstate-key (append superstate-key (list ,name))))
     (progn ,@body)))

(defmacro with-new-selector (name type &body body)
  `(let* ((current-selector (make-instance ',type
			     :selected-state ,name)))
     (progn ,@body)))


(defmacro %superstate (type name state-selector default-state description entry exit sub-states)
  `(,state-selector
    ,default-state
    (statecharts::%register-state
     statechart superstate-key ,name 
     (with-new-superstate ,name
       (make-instance ',type :name ,name :description ,description
			     :on-entry (if ,entry ,entry (constantly t))
			     :on-exit (if ,exit ,exit (constantly t))
			     :selector current-selector
			     :elements (list ,@sub-states))))))

;;; statechart definition language

(defmacro -> (event initial final &key if)
  "Given event with name EVENT, construct an object of type TRANSITION
from initial state with name INITIAL to final state with name
FINAL. If IF is non-nil, it is assumed to be a function of one
parameter, the statechart ENVIRONMENT and the transition will only
proceed if IF returns true.

==> TRANSITION"
  `(let* ((initial-key (statecharts::%dereference-key superstate-key ,initial))
	  (final-key (statecharts::%dereference-key superstate-key ,final))
	  (trans-obj (statecharts::%t initial-key ,event final-key ,if)))
     (statecharts::%register-event statechart ,event trans-obj)
     (statecharts::%register-transition statechart initial-key final-key trans-obj)))

(defmacro d (default-state &body body)
  "Returns an object of type DEFAULT-SELECTOR that selects the state
with name DEFAULT-STATE

==> DEFAULT-SELECTOR"
  `(with-new-selector ,default-state statecharts::default-selector
     ,@body))

(defmacro h (initial-state &body body)
  "Returns an object of type HISTORY-SELECTOR that keeps a record and
selects the last recorded state upon entry. Upon first entry the state
wih name INITIAL-STATE will be selected.

==> HISTORY-SELECTOR"
  `(with-new-selector ,initial-state statecharts::history-selector
     ,@body))


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
  `(%superstate orhtogonal ,name ,state-selector ,default-state
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
  `(statecharts::%register-state statechart superstate-key ,name 
				 (statecharts::%s ,name ,description ,entry ,exit
						  current-selector)))


(defmacro defstatechart ((name &key (description "")) &body definitions)
  (%check-defstatechart-arguments name description definitions)
  `(let* ((statechart (make-instance 'statecharts::statechart :name ,(string name)
							      :description ,description))
	  (superstate-key '())
	  (current-selector))
     (progn ,@definitions)
     (defparameter ,name statechart)
     statechart))









