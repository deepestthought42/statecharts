;;;; statecharts.lisp

(defpackage #:statecharts.dsl
  (:use #:cl #:iterate #:let-plus #:trivia #:sc.cond)
  (:nicknames #:sc.dsl)
  (:export #:->
	   #:o
	   #:c
	   #:s
	   #:d
	   #:if-in-state
	   #:act
	   #:when-in-state))

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
      (error "Must initialize name for statechart-element."))
  (setf (name obj) (alexandria:make-keyword (name obj))))



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
   (event-symbol :accessor event-symbol :reader event-symbol)))


(defmethod initialize-instance :after ((obj transition) &key)
  (setf (event-symbol obj) (alexandria:make-keyword (event obj))
	(initial-state obj)
	(cond
	  ((listp (initial-state obj))
	   (mapcar #'alexandria:make-keyword (initial-state obj)))
	  (t (alexandria:make-keyword (initial-state obj))))))



(defun make-transition (initial-name event clauses)
  (labels ((format-name (name)
	     (cond
	       ((or (symbolp name) (stringp name)) name)
	       ((listp name) (format nil "~{~a~^::~}" name))
	       (t (error "This shouldn't happen.")))))
    (make-instance 'sc.dsl::transition
		   :name (format nil "~a -> (~{~a~^,~})"
				 (format-name initial-name)
				 (mapcar #'(lambda (c) (format-name (final-state c))) clauses))
		   :event event
		   :initial-state initial-name
		   :clauses clauses)))

(defclass transition-clause ()
  ((final-state :accessor final-state :initarg :final-state
		:initform (error "Must initialize FINAL-STATE."))))



(defclass when-in-state-clause (transition-clause)
  ((in-state :accessor in-state :initarg :in-state
	     :initform (error "Need to initialize IN-STATE."))))


(defclass otherwise-clause (transition-clause) ())

(defclass guard-clause (transition-clause)
  ((code :accessor code :initarg :code
	 :initform (error "Must initialize CODE."))
   (fun :accessor fun :initarg :fun
	:initform (error "Need to initialize FUN."))))


(defgeneric clause-function (clause)
  (:method ((clause transition-clause)) (constantly t))
  (:method ((clause guard-clause)) (fun clause)))


(sc.utils::define-copy-object-method (transition-clause) final-state)
(sc.utils::define-copy-object-method (when-in-state-clause) final-state in-state)
(sc.utils::define-copy-object-method (otherwise-clause) final-state)
(sc.utils::define-copy-object-method (guard-clause) final-state code fun)


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
		:initform '())
   (is-substate-of-cluster :accessor is-substate-of-cluster
			   :initarg :is-substate-of-cluster
			   :initform nil)))

(defclass state-selector (statechart-element)
  ((selected-state :initarg :selected-state :accessor selected-state 
		   :initform (error "Must initialize selected-state."))))

(defmethod initialize-instance :after ((ss state-selector) &key)
  (setf (selected-state ss) (alexandria:make-keyword (selected-state ss))))

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


(defmethod initialize-instance :after ((sws cluster) &key)
  (setf (default-state sws) (alexandria:make-keyword (default-state sws))))

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
    (sc.utils::%print-object obj stream)))

(defmethod sc.utils::%print-object ((obj transition-clause) stream)
  (format stream "~a" (sc.key::name (final-state obj))))

(defmethod sc.utils::%print-object ((obj otherwise-clause) stream)
  (format stream "otherwise -> ~a)" (sc.key::name (final-state obj))))

(defmethod sc.utils::%print-object ((obj guard-clause) stream)
  (format stream "~a -> ~a"
	  (code obj)
	  (sc.key::name (final-state obj))))


