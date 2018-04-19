(in-package #:statecharts)


(defclass fsm-state ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))
   (ev->state :accessor ev->state :initarg :ev->state
	      :initform '())))

(defmethod print-object ((obj fsm-state) stream)
  (print-unreadable-object (obj stream)
    (format stream "fsm-state w/name: ")
    (print-state-name (name obj) stream)))



(defun create-fsm-states (states)
  (iter
    (for s in states)
    (collect (make-instance 'fsm-state :name (create-state-name s)))))













