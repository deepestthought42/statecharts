;;;; statecharts.lisp

(defpackage #:statecharts.dsl
  (:use #:cl #:iterate #:let-plus #:trivia)
  (:nicknames #:dsl))

(in-package #:statecharts.dsl)

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



(defclass transition (statechart-element)
  ((event :initarg :event :accessor event 
	  :initform (error "Must initialize event."))
   (initial-state :initarg :initial-state :accessor initial-state 
		  :initform (error "Must initialize initial-state."))
   (clauses :accessor clauses :initarg :clauses
	    :initform (error "Need to initialize CLAUSES."))
   (event-symbol :reader event-symbol)))


(defmethod initialize-instance :after ((obj transition) &key)
  (setf (slot-value obj 'event-symbol)
	(alexandria:symbolicate (event obj))))



(defun make-transition (initial-name event clauses)
  (labels ((format-name (name)
	     (cond
	       ((stringp name) name)
	       ((listp name) (format nil "~{~a~^::~}" name))
	       (t (error "This should happen.")))))
    (make-instance 'dsl::transition
		   :name (format nil "~a -> (~{~a~^,~})"
				 (format-name initial-name)
				 (mapcar #'(lambda (c) (format-name (final-state c))) clauses))
		   :event event
		   :initial-state initial-name
		   :clauses clauses)))

(defclass transition-clause ()
  ((final-state :accessor final-state :initarg :final-state
		:initform (error "Must initialize FINAL-STATE."))))

(defclass otherwise-clause (transition-clause) ())

(defclass guard-clause (transition-clause)
  ((code :accessor code :initarg :code
	 :initform (error "Must initialize CODE."))
   (fun :accessor fun :initarg :fun
	:initform (error "Need to initialize FUN."))))


(sc::define-copy-object-method (transition-clause) final-state)
(sc::define-copy-object-method (otherwise-clause) final-state)
(sc::define-copy-object-method (guard-clause) final-state code fun)


(defgeneric applicable (target environment)
  (:method ((clause transition-clause) environment) t)
  (:method ((clause guard-clause) environment)
    (funcall (fun clause) environment)))

(defclass action ()
  ((fun :accessor fun :initarg :fun :initform (constantly t))
   (code :accessor code :initarg :code :initform '())))

(defclass activity ()
  ((start-fun :accessor start-fun :initarg :start-fun :initform (constantly t))
   (stop-fun :accessor stop-fun :initarg :stop-fun :initform (constantly t))))

(defclass state (statechart-element)
  ((on-entry :initarg :on-entry
	     :accessor on-entry 
	     :initform '())
   (on-reentry :initarg :on-reentry
	       :accessor on-reentry 
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


(defmethod print-obect ((obj transition-clause) stream)
  (print-unreadable-object (obj stream)
    (sc::%print-object obj stream)))

(defmethod sc::%print-object ((obj transition-clause) stream)
  (format stream "~a" (name::name (final-state obj))))

(defmethod sc::%print-object ((obj otherwise-clause) stream)
  (format stream "(otherwise -> ~a)" (name::name (final-state obj))))

(defmethod sc::%print-object ((obj guard-clause) stream)
  (format stream "(if ~a -> ~a)"
	  (code obj)
	  (name::name (final-state obj))))


