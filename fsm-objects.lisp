(defpackage #:statecharts.fsm
  (:use #:cl #:iterate #:let-plus #:sc.cond)
  (:nicknames #:sc.fsm)
  (:import-from #:sc.dsl #:fun)
  (:export
   #:environment
   #:signal-event
   #:create-fsm-runtime))

(in-package #:sc.fsm)

(defclass state ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))
   (ev->state :accessor ev->state :initarg :ev->state
	      :initform '())
   (targets-with-history :accessor targets-with-history :initarg :targets-with-history
			 :initform '())))


(defmethod initialize-instance :after ((state state) &key)
  (setf (name state) (sc.utils::create-hashed (name state))))

(defmethod print-object ((obj state) stream)
  (print-unreadable-object (obj stream)
    (format stream "state w/name: ")
    (sc.utils::%print-object (name obj) stream)))


(defclass transition ()
  ((initial-state-name :initarg :initial-state-name :accessor initial-state-name 
		       :initform (error "Must initialize initial-state-name."))
   (final-state-name :accessor final-state-name :initarg :final-state-name
		     :initform (error "Need to initialize FINAL-STATE-NAME."))
   (event-name :initarg :event-name :accessor event-name 
	       :initform (error "Must initialize event-state-name."))
   (clause :accessor clause :initarg :clause
	   :initform (error "Need to initialize CLAUSE."))))

(defclass target ()
  ((on-entry-actions :initarg :on-entry-actions :accessor on-entry-actions
		     :initform (error "Must initialize on-entry-actions."))
   (on-reentry-actions :initarg :on-reentry-actions :accessor on-reentry-actions
		       :initform (error "Must initialize on-reentry-actions."))
   (on-exit-actions :initarg :on-exit-actions :accessor on-exit-actions
		    :initform (error "Must initialize on-exit-actions."))
   (initial-name :initarg :initial-name :accessor initial-name
		 :initform (error "Must initialize initial-name."))
   (final-name :initarg :final-name :accessor final-name
 	       :initform (error "Must initialize final-name."))
   (state :initarg :state :accessor state
	  :initform (error "Must initialize state."))))

(defmethod print-object ((obj target) stream)
  (print-unreadable-object (obj stream)
    (format stream "-> ")
    (sc.utils::%print-object (name (state obj)) stream)))

(defgeneric applicable (target environment)
  (:method ((target target) environment)
    (declare (ignorable target environment))
    t))


(defclass guarded-target (target)
  ((clauses :accessor clauses :initarg :clauses
	    :initform (error "Need to initialize CLAUSES."))))


(defgeneric update-history (target new-fsm-state)
  (:method ((target target) (new-fsm-state state))
    (setf (state target) new-fsm-state)))


(defmethod applicable ((target guarded-target) environment)
  (iter
    (for g in (clauses target))
    (when (not (sc.dsl::applicable g environment))
      (return nil))
    (finally (return t))))


(defun default-error-handler (fun environment)
  (with-simple-restart (ignore "Ignore error")
    (funcall fun environment)))


(defclass environment ()
  ((fsm :initarg :fsm :reader fsm)
   (error-handler :accessor error-handler :initarg :error-handler))
  (:default-initargs
   :fsm (error "Must initialize fsm.")
   :error-handler #'default-error-handler))

