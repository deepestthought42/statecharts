(defpackage #:statecharts.state-key
  (:use #:cl #:iterate #:let-plus)
  (:nicknames #:key #:name))


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


(defmethod print-object ((obj state) stream)
  (print-unreadable-object (obj stream)
    (format stream "s: ")
    (sc::%print-object obj stream)))

(defmethod sc::%print-object ((obj state) stream)
  (format stream "(~a)" (name obj)))

(defmethod sc::%print-object ((obj or-state) stream)
  (format stream "(~a" (name obj))
  (when (sub-state obj)
    (format stream " ")
    (sc::%print-object (sub-state obj) stream))
  (format stream ")"))


(defmethod sc::%print-object ((obj and-state) stream)
  (format stream "(~a" (name obj))
  (let ((sub-states (sub-states obj)))
    (when sub-states
      (format stream " ")
      (sc::%print-object (car sub-states) stream)
      (map nil #'(lambda (s)
		   (format stream "∧")
		   (sc::%print-object s stream))
	   (cdr sub-states))))
  (format stream ")"))



