;;;; statecharts.lisp

(in-package #:statecharts)

;;; "statecharts" goes here. Hacks and glory await!

(in-package #:statechart)

;;; "statechart" goes here. Hacks and glory await!

(defclass event ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))))

(defclass action ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))))


(defclass state ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))
   (description :accessor description :initarg :description :initform "")
   
   (on-entry :initarg :on-entry :accessor on-entry 
	     :initform '())
   (on-exit :initarg :on-exit :accessor on-exit 
	    :initform '())))





