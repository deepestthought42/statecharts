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
  (let+ (((&slots current-state environment) runtime)
	 ((&slots ev->state on-exit) current-state)
	 (ev.target (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (labels ((execute-actions (actions category)
	       (declare (ignore category))
	       (iter
		 (for act in actions)
		 (funcall (fun act) environment))))
      (cond
	((not ev.target)
	 (return-from signal-event nil))
	(t (let+ (((&slots fsm-state on-exit-actions on-entry-actions)
		   (cdr ev.target)))
	     (execute-actions on-exit-actions :actions)
	     (setf current-state fsm-state)
	     (execute-actions on-entry-actions :actions)))))))

(defmethod signal-event ((runtime debug-statecharts-runtime-fsm) event)
  (let+ (((&slots current-state debug-fn environment) runtime)
	 ((&slots ev->state on-exit) current-state)
	 (ev.target (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (labels ((dbgout (cat str &rest args)
	       (apply debug-fn cat str args))
	     (execute-actions (actions category)
	       (iter
		 (for act in actions)
		 (dbgout category "Executing: ~a" (name act))
		 (funcall (fun act) environment))))
      (dbgout :signal-event "Received event: ~a" event)
      (cond
	((not ev.target)
	 (dbgout :signal-event "Not reacting on event: ~a in state: ~a" event (name current-state))
	 (return-from signal-event nil))
	(t (let+ (((&slots fsm-state on-exit-actions on-entry-actions)
		   (cdr ev.target)))
	     (dbgout :signal-event "Leaving state: ~a" (name current-state))
	     (execute-actions on-exit-actions :actions)
	     (setf current-state fsm-state)
	     (dbgout :signal-event "Entered state: ~a" (name current-state))
	     (execute-actions on-entry-actions :actions)))))))



(defun create-fsm-runtime (statechart &key debug)
  (let+ (((&slots events fsm-states environment-type default-state) statechart)
	 (default-fsm-state (find (create-state-name default-state)
				  fsm-states :test #'state-name=
				  :key #'name)))
    (if (not default-fsm-state)
	(error "Huh ? Couldn't find default state: ~a ?" default-state))
    (make-instance (if debug
		       'debug-statecharts-runtime-fsm
		       'statecharts-runtime-fsm)
		   :default-state default-fsm-state
		   :states fsm-states
		   :current-state default-fsm-state
		   :events events
		   :environment-type environment-type)))
