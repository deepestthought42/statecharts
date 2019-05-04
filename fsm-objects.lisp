(defpackage #:statecharts.fsm
  (:use #:cl #:iterate #:let-plus)
  (:nicknames #:fsm)
  (:import-from dsl #:fun)
  (:export
   #:environment))

(in-package #:fsm)

(defclass state ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))
   (ev->state :accessor ev->state :initarg :ev->state
	      :initform '())))

(defmethod print-object ((obj  state) stream)
  (print-unreadable-object (obj stream)
    (format stream " state w/name: ")
    (sc::%print-object (name obj) stream)))


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


(defgeneric applicable (target environment)
  (:method ((target target) environment) t))


(defclass guarded-target (target)
  ((guards :accessor guards :initarg :guards
	   :initform (error "Need to initialize GUARDS."))))

#+nil
(defmethod applicable ((target guarded-target) environment)
  (iter
    (for g in (guards target))
    (when (not (applicable g environment))
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

