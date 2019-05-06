(defpackage #:statecharts
  (:use #:cl #:iterate #:let-plus #:trivia #:sc.cond #:sc.dsl #:sc.fsm)
  (:nicknames #:sc)
  (:export
   #:->
   #:o
   #:c
   #:s
   #:d
   #:act
   #:environment
   #:signal-event
   #:create-fsm-runtime
   #:defstatechart
   #:render))


(in-package #:statecharts)

(defclass statechart ()
  ((name :accessor name :initarg :name :initform (error "Need to initialize NAME."))
   (description :accessor description :initarg :description
		:initform (error "Need to initialize DESCRIPTION."))
   (events :accessor events :initarg :events :initform nil)
   (root :accessor root :initarg :root :initform nil)
   (states :accessor states :initarg :states :initform '())
   (fsm-states :accessor fsm-states :initarg :fsm-states :initform '())
   (default-state :accessor default-state :initarg :default-state :initform nil)
   (transitions :accessor transitions :initarg :transitions :initform '())))
