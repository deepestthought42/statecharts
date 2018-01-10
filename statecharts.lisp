;;;; statecharts.lisp

(in-package #:statecharts)

;;; "statecharts" goes here. Hacks and glory await!



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


(defclass statechart (statechart-element)
  ((root :accessor root :initarg :root :initform nil)
   (states :accessor states :initarg :states :initform '())
   (transitions :accessor transitions :initarg :transitions :initform '())
   (events :accessor events :initarg :events :initform '())))

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


(defclass action ()
  ((fun :accessor fun :initarg :fun :initform (constantly t))))

(defparameter *nothing* (make-instance 'action))

(defclass state (statechart-element)
  ((on-entry :initarg :on-entry
	     :accessor on-entry 
	     :initform '())
   (on-exit :initarg :on-exit
	    :accessor on-exit 
	    :initform '())
   (transitions :accessor transitions
		:initarg :transitions
		:initform '())))

(defclass state-selector (statechart-element)
  ((selected-state :initarg :selected-state :accessor selected-state 
		   :initform (error "Must initialize selected-state."))))


(defclass history-selector (state-selector) ()
  (:default-initargs :name "History selector"))


(defclass default-selector (state-selector) ()
  (:default-initargs :name "Default state selector"))


(defclass cluster (state)
  ((selector-type :initarg :selector-type :accessor selector-type 
		  :initform (error "Must initialize selector-type."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))
   (elements :initarg :elements :accessor elements 
	     :initform (error "Must initialize elements."))))

(defclass orthogonal (cluster) ())


