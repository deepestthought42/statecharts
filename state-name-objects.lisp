(defpackage #:statecharts.state-key
  (:use #:cl #:iterate #:let-plus #:sc.cond)
  (:nicknames #:sc.key #:sc.name))


(in-package #:statecharts.state-key)

;;; need to have a better way to represent states internally than a conses


(defclass state-id ()
  ((state-bits :accessor state-bits :initarg :state-bits
	       :initform (error "Need to initialize STATE-BITS."))
   (excluded-states-bits :accessor excluded-state-bits :initarg :excluded-state-bits
			 :initform 0)
   (defining-element :accessor defining-element
		     :initarg :defining-element
		     :initform (error "Need to initialize DEFINING-ELEMENT."))))

(defclass state (state-id)
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))))

(defclass state-with-substates (state)  ())

(defclass or-state (state-with-substates)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))))

(defclass and-state (state-with-substates)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))

(defmethod initialize-instance :after ((obj and-state) &key)
  (setf (sub-states obj) (sort (sub-states obj) #'string< :key #'name)))





