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


(defclass event (statechart-element) ())

(defparameter *no-event* nil)
(defparameter *unknown* nil)

(defparameter *default-hash-table-size* 1000)

(defun default-hashtable ()
  (make-hash-table :test #'equal :size *default-hash-table-size*))

(defclass statechart (statechart-element)
  ((states :accessor states :initarg :states :initform (default-hashtable))
   (transitions :accessor transitions :initarg :transitions :initform (default-hashtable))
   (events :accessor events :initarg :events :initform (default-hashtable))))


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
	    :initform (constantly t))))

(defclass history (state) ())

(defparameter *history-types* '(:same-level-only :all-deeper-levels))

(defclass cluster (statechart-element)
  ((default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))
   
   (elements :initarg :elements :accessor elements 
	     :initform (error "Must initialize elements."))))

(defclass orthogonal (cluster) ())


