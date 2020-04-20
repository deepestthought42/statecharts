(defpackage #:statecharts.state-key
  (:use #:cl #:iterate #:let-plus #:sc.cond)
  (:nicknames #:sc.key #:sc.name))


(in-package #:statecharts.state-key)

;;; need to have a better way to represent states internally than a conses

(defclass state ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))))

(defclass or-state (state)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))))

(defclass and-state (state)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))

(defmethod initialize-instance :after ((obj and-state) &key)
  (setf (sub-states obj) (sort (sub-states obj) #'string< :key #'name)))


(defmethod print-object ((obj state) stream)
  (print-unreadable-object (obj stream)
    (format stream "s: ")
    (sc.utils::%print-object obj stream)))

(defmethod sc.utils::%print-object ((obj state) stream)
  (format stream "(~a)" (name obj)))

(defmethod sc.utils::%print-object ((obj or-state) stream)
  (format stream "(~a" (name obj))
  (when (sub-state obj)
    (format stream " ")
    (sc.utils::%print-object (sub-state obj) stream))
  (format stream ")"))


(defmethod sc.utils::%print-object ((obj and-state) stream)
  (format stream "(~a" (name obj))
  (let ((sub-states (sub-states obj)))
    (when sub-states
      (format stream " ")
      (sc.utils::%print-object (car sub-states) stream)
      (map nil #'(lambda (s)
		   (format stream "∧")
		   (sc.utils::%print-object s stream))
	   (cdr sub-states))))
  (format stream ")"))


(defstruct state-id
  (id 0 :type (integer 0))
  (and-sub-states '() :type list))

