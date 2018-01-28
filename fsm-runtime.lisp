(in-package #:statecharts)


(defclass statecharts-runtime ()
  ((enviroment :initarg :enviroment :accessor enviroment 
	       :initform (error "Must initialize enviroment."))
   (current-state :initarg :current-state :accessor current-state 
		  :initform (error "Must initialize current-state."))
   (states :initarg :states :accessor states 
	   :initform (error "Must initialize states."))
   (events :initarg :events :accessor events 
	   :initform (error "Must initialize events."))
   (event->state :initarg :event->state :accessor event->state 
		 :initform (error "Must initialize event->state."))))



(defmethod set-next-state ((runtime statecharts-runtime) next-state)
  (setf (current-state runtime) next-state))

(defmethod signal-event ((runtime statecharts-runtime) event)
  (run-on-exit))
