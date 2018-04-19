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
	 ((&slots ev->state) current-state)
	 ((&slots state actions) (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (cond
      ((not state)
       (return-from signal-event nil))
      (t (setf state (cdr state))))
    (labels ((execute-actions (actions)
	       (iter
		 (for act in actions)
		 (funcall (fun act)))))
      (execute-actions actions)
      (setf current-state state))))


(defmethod signal-event ((runtime debug-statecharts-runtime-fsm) event)
  (let+ (((&slots current-state debug-fn) runtime)
	 ((&slots ev->state on-exit) current-state)
	 ((&slots state actions) (assoc event ev->state)))
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
	((not state)
	 (dbgout :signal-event "Not reacting on event: ~a in state: ~a" event (name current-state))
	 (return-from signal-event nil))
	(t (setf state (cdr state))))
      (dbgout :signal-event "Leaving state: ~a" (name current-state))
      (execute-actions actions :actions)
      (setf current-state state)
      (dbgout :signal-event "Entered state: ~a" (name state)))))



(defun create-fsm-runtime (statechart &key debug)
  (let+ (((&slots states events transitions environment-type default-state) statechart)
	 (fsm-states (replace-final-states-in-transitions (create-fsm-states states)))
	 (flattened-default-state (find-flattened-default-state default-state fsm-states)))
    (make-instance (if debug
		       'debug-statecharts-runtime-fsm
		       'statecharts-runtime-fsm)
		   :default-state flattened-default-state
		   :states fsm-states
		   :current-state flattened-default-state
		   :events events
		   :environment-type environment-type)))
