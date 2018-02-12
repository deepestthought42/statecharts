(in-package #:statecharts)


(defclass statecharts-runtime-fsm ()
  ((environment-type :initarg :environment-type :accessor environment-type 
		     :initform (error "Must initialize environment-type."))
   (environment :accessor environment)
   (current-state :initarg :current-state :accessor current-state 
		  :initform (error "Must initialize current-state."))
   (states :initarg :states :accessor states 
	   :initform (error "Must initialize states."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))
   (events :initarg :events :accessor events 
	   :initform (error "Must initialize events."))))

(defclass debug-statecharts-runtime-fsm (statecharts-runtime-fsm)
  ((debug-fn :accessor debug-fn :initarg :debug-fn
	     :initform #'(lambda (cat str &rest args)
			   (apply #'format t
				  (format nil "[~a] ~a~%" cat str)
				  args)))))

(defmethod initialize-instance :after ((obj statecharts-runtime-fsm) &key)
  (setf (environment obj)
	(make-instance (environment-type obj) :fsm obj)))


(defgeneric signal-event (runtime event))

(defmethod signal-event ((obj environment) event)
  (signal-event (fsm obj) event))

(defmethod signal-event ((runtime statecharts-runtime-fsm) event)
  (let+ (((&slots current-state) runtime)
	 ((&slots ev->state on-exit) current-state)
	 (->state (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (cond
      ((not ->state)
       (return-from signal-event nil))
      (t (setf ->state (cdr ->state))))
    (labels ((execute-actions (actions)
	       (iter
		 (for act in actions)
		 (funcall (fun act)))))
      (execute-actions (on-exit current-state))
      (setf current-state ->state)
      (execute-actions (on-entry ->state)))))


(defmethod signal-event ((runtime debug-statecharts-runtime-fsm) event)
  (let+ (((&slots current-state debug-fn) runtime)
	 ((&slots ev->state on-exit) current-state)
	 (->state (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (labels ((dbgout (cat str &rest args)
	       (apply debug-fn cat str args))
	     (execute-actions (actions category)
	       (iter
		 (for act in actions)
		 (dbgout category "Executing: ~a" (name act))
		 (funcall (fun act)))))
      (dbgout :signal-event "Received event: ~a" event)
      (cond
	((not ->state)
	 (dbgout :signal-event "Not reacting on event: ~a in state: ~a" event (name current-state))
	 (return-from signal-event nil))
	(t (setf ->state (cdr ->state))))
      (dbgout :signal-event "Leaving state: ~a" (name current-state))
      (execute-actions (on-exit current-state) :on-exit)
      (dbgout :signal-event "Entering state: ~a" (name ->state))
      (setf current-state ->state)
      (execute-actions (on-entry ->state) :on-entry))))



(defun create-fsm-runtime (statechart &key debug)
  (let+ (((&slots states events transitions environment-type default-state) statechart)
	 (states.flattened-states (flatten-all-states states))
	 (flattened-states (replace-final-states-in-transitions states.flattened-states))
	 (flattened-default-state
	  (find-flattened-default-state default-state states.flattened-states)))
    (make-instance (if debug
		       'debug-statecharts-runtime-fsm
		       'statecharts-runtime-fsm)
		   :default-state flattened-default-state
		   :states flattened-states
		   :current-state flattened-default-state
		   :events events
		   :environment-type environment-type)))
