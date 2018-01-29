(in-package #:statecharts)


(defclass statecharts-runtime-fsm ()
  ((enviroment-type :initarg :enviroment-type :accessor enviroment-type 
		    :initform (error "Must initialize enviroment-type."))
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
  (let+ (((&slots current-state enviroment) runtime)
	 ((&slots ev->state on-exit) current-state)
	 ((&ign . ->state) (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (cond
      ((not ->state)
       (return-from signal-event nil))
      (t (setf ->state (cdr ->state))))
    (labels ((execute-actions (actions)
	       (iter
		 (for act in actions)
		 (funcall (fun act) enviroment))))
      (execute-actions (on-exit current-state))
      (setf current-state ->state)
      (execute-actions (on-entry ->state)))))


(defmethod signal-event ((runtime debug-statecharts-runtime-fsm) event)
  (let+ (((&slots current-state enviroment debug-fn) runtime)
	 ((&slots ev->state on-exit) current-state)
	 (->state (assoc event ev->state)))
    ;; fixmee: guards not implemented yet
    (labels ((dbgout (cat str &rest args)
	       (funcall debug-fn cat str args))
	     (execute-actions (actions category)
	       (iter
		 (for act in actions)
		 (dbgout category "Executing:" (name act))
		 (funcall (fun act) enviroment))))
      (cond
	((not ->state)
	 (dbgout :signal-event "Couldn't find final state for event: ~a" event)
	 (return-from signal-event nil))
	(t (setf ->state (cdr ->state))))
      (dbgout :signal-event "Leaving state: ~a" (name current-state))
      (execute-actions (on-exit current-state) :on-exit)
      (dbgout :signal-event "Entering state: ~a" (name ->state))
      (setf current-state ->state)
      (execute-actions (on-entry ->state) :on-entry))))
