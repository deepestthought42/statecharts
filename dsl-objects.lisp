;;;; statecharts.lisp

(in-package #:statecharts)

;;; objects the statechart DSL evaluates into

(let ((current-id 0))
  (defun get-id () (incf current-id))
  (defun clear-id () (setf current-id 0)))


(defclass statechart-element ()
  ((description :accessor description :initarg :description :initform "")
   (name :initarg :name :accessor name :initform nil)
   (id :reader id :initform (get-id))))


(defmethod initialize-instance :after ((obj statechart-element) &key)
  (if (not (name obj))
      (error "Must initialize name for statechart-element.")))


(defclass event (statechart-element)
  ((registered-transitions :accessor registered-transitions :initarg :registered-transitions
			   :initform '())))



(defclass statechart (statechart-element)
  ((root :accessor root :initarg :root :initform nil)
   (states :accessor states :initarg :states :initform '())
   (fsm-states :accessor fsm-states :initarg :fsm-states :initform '())
   (default-state :accessor default-state :initarg :default-state :initform nil)
   (transitions :accessor transitions :initarg :transitions :initform '())
   (events :accessor events :initarg :events :initform '())))

(defclass transition (statechart-element)
  ((event :initarg :event :accessor event 
	  :initform (error "Must initialize event."))
   (guards :accessor guards :initarg :guards :initform '())
   (initial-state :initarg :initial-state :accessor initial-state 
		  :initform (error "Must initialize initial-state."))
   (final-state :initarg :final-state :accessor final-state 
		:initform (error "Must initialize final-state."))
   (event-symbol :reader event-symbol)))

(defmethod initialize-instance :after ((obj transition) &key)
  (setf (slot-value obj 'event-symbol)
	(alexandria:symbolicate (event obj))))


(defclass conditional ()
  ((description :initarg :description :accessor description :initform "")))

(defclass in-state (conditional)
  ((state-name :initarg :state-name :accessor state-name 
	       :initform (error "Must initialize state-name."))))


(defclass action ()
  ((fun :accessor fun :initarg :fun :initform (constantly t))))

(defclass activity ()
  ((start-fun :accessor start-fun :initarg :start-fun :initform (constantly t))
   (stop-fun :accessor stop-fun :initarg :stop-fun :initform (constantly t))))

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

(defclass state-with-substates (state)
  ((elements :initarg :elements :accessor elements 
	     :initform (error "Must initialize elements."))))


(defclass cluster (state-with-substates)
  ((selector-type :initarg :selector-type :accessor selector-type 
		  :initform (error "Must initialize selector-type."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))))


(defclass orthogonal (state-with-substates) ())


(defmethod print-object ((obj state) stream)
  (print-unreadable-object (obj stream)
    (format stream "s: ~a" (name obj))))

(defmethod print-object ((obj cluster) stream)
  (print-unreadable-object (obj stream)
    (format stream "c: ~a" (name obj))))

(defmethod print-object ((obj orthogonal) stream)
  (print-unreadable-object (obj stream)
    (format stream "o: ~a" (name obj))))

(defclass environment ()
  ((fsm :initarg :fsm :reader fsm))
  (:default-initargs
   :fsm (error "Must initialize fsm.")))

(defgeneric create-environment (environment))



