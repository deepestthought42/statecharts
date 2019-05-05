(in-package #:sc.fsm)


(defclass statecharts-runtime-fsm ()
  ((current-state :initarg :current-state :accessor current-state 
		  :initform (error "Must initialize current-state."))
   (states :initarg :states :accessor states 
	   :initform (error "Must initialize states."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))
   (events :initarg :events :accessor events 
	   :initform (error "Must initialize events."))
   (event-queue :accessor event-queue :initarg :event-queue)
   (processing :accessor processing :initarg :processing))
  (:default-initargs
   :processing nil
   :event-queue (queues:make-queue :simple-queue)))


(defclass debug-statecharts-runtime-fsm (statecharts-runtime-fsm)
  ((debug-fn :accessor debug-fn :initarg :debug-fn
	     :initform #'(lambda (cat str &rest args)
			   (apply #'format t
				  (format nil "[~a] ~a~%" cat str)
				  args)))))



(defun %%signal-event (current-state debug-fn event environment)
  (let+ (((&slots ev->state on-exit) current-state)
	 (ev/target* (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (labels ((dbgout (cat str &rest args)
	       (apply debug-fn cat str args))
	     (execute-actions (actions category)
	       (iter
		 (for act in actions)
		 (dbgout category "Executing: ~a" (documentation (fun act) 'function))

		 (funcall (error-handler environment)
			  (fun act) environment))))
      (dbgout :signal-event "Received event: ~a" event)
      (cond
	((not ev/target*)
	 (dbgout :signal-event "Not reacting on event: ~a in state: ~a" event (name current-state))
	 (return-from %%signal-event current-state))
	(t
	 (iter
	   (for target in (cdr ev/target*))
	   (when (applicable target environment)
	     (let+ (((&slots state on-exit-actions on-entry-actions on-reentry-actions) target))
	       (dbgout :signal-event "Leaving state: ~a" (name current-state))
	       (execute-actions on-exit-actions :exit-actions) 
	       (dbgout :signal-event "Entered state: ~a" (name state))
	       (execute-actions on-entry-actions :entry-actions)
	       (execute-actions on-reentry-actions :reentry-actions)
	       (return state)))
	   (finally (return current-state))))))))


(defmethod %signal-event ((runtime debug-statecharts-runtime-fsm) event environment)
  (let+ (((&slots current-state debug-fn) runtime))
    (setf current-state
	  (%%signal-event current-state debug-fn event environment))))

(defmethod %signal-event ((runtime statecharts-runtime-fsm) event environment)
  (let+ (((&slots current-state) runtime))
    (setf current-state
	  (%%signal-event current-state (constantly nil) event environment))))

(defgeneric signal-event (environment event &key))

(defmethod signal-event ((environment environment) new-event
			 &key (fsm (fsm environment)))
  (let+ (((&slots event-queue processing) fsm))
    (queues:qpush event-queue new-event)
    (unless processing
      (unwind-protect 
	  (progn
	    (setf processing t)
	    (iter
	      (for (values event event-returned)
		   = (queues:qpop event-queue))
	      (while event-returned)
	      (%signal-event fsm event environment)))
	(setf processing nil)))))




(defun create-fsm-runtime (statechart &key debug)
  (let+ (((&slots sc::events sc::fsm-states sc::default-state) statechart)
	 (default-fsm/state (find (sc.key::from-chart-state sc::default-state)
				  sc::fsm-states :test #'sc.name::state=
				  :key #'name)))
    (if (not default-fsm/state)
	(error "Huh ? Couldn't find default state: ~a ?" sc::default-state))
    (make-instance (if debug
		       'debug-statecharts-runtime-fsm
		       'statecharts-runtime-fsm)
		   :default-state default-fsm/state
		   :states sc::fsm-states
		   :current-state default-fsm/state
		   :events sc::events)))
