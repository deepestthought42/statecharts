;;;; statecharts.lisp

(in-package #:statecharts)

;;; "statecharts" goes here. Hacks and glory await!


(defparameter *default-environment* (make-instance 'environment))


(defclass statechart-element ()
  ((description :accessor description :initarg :description
		:initform "")
   (name :initarg :name :accessor name :initform nil)))


(defmethod initialize-instance :after ((obj statechart-element) &key)
  (if (not (name obj))
      (error "Must initialize name for statechart-element.")))


(defclass event (statechart-element)
  ((registered-transitions :accessor registered-transitions :initarg :registered-transitions
			   :initform '())))

(defparameter *no-event* nil)
(defparameter *unknown* nil)

(defparameter *default-hash-table-size* 1000)

(defun default-hashtable ()
  (make-hash-table :test #'equal :size *default-hash-table-size*))

(defclass statechart (statechart-element)
  ((states :accessor states :initarg :states :initform (default-hashtable))
   (transitions :accessor transitions :initarg :transitions :initform (default-hashtable))
   (events :accessor events :initarg :events :initform (default-hashtable))))

(defclass environment () ())

(defclass transition (statechart-element)
  ((event :initarg :event :accessor event 
	  :initform (error "Must initialize event."))
   (guard :initarg :guard :accessor guard 
	  :initform (constantly t))
   (initial-state :initarg :initial-state :accessor initial-state 
		  :initform (error "Must initialize initial-state."))
   (final-state :initarg :final-state :accessor final-state 
		:initform (error "Must initialize final-state."))))


(defclass action () ())

(defclass state (statechart-element)
  ((on-entry :initarg :on-entry
	     :accessor on-entry 
	     :initform (constantly t))
   (on-exit :initarg :on-exit
	    :accessor on-exit 
	    :initform (constantly t))
   (selector :initarg :selector :accessor selector 
	     :initform (error "Must initialize selector."))))



(defclass state-selector (statechart-element)
  ((selected-state :initarg :selected-state :accessor selected-state 
		   :initform (error "Must initialize selected-state."))))


(defclass history-selector (state-selector) ()
  (:default-initargs :name "History selector"))


(defclass default-selector (state-selector) ()
  (:default-initargs :name "Default state selector"))


(defclass cluster (state)
  ((elements :initarg :elements :accessor elements 
	     :initform (error "Must initialize elements."))))

(defclass orthogonal (cluster) ())


