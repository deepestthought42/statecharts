(in-package #:statecharts)


(define-condition state-exists (error)
  ((name :accessor name :initarg :name :initform "unspecified")))

(define-condition transition-exists (error)
  ((name :accessor name :initarg :name :initform "unspecified")))

(define-condition invalid-state-descriptor (error)
  ((descriptor :accessor descriptor :initarg :descriptor
	       :initform "unspecified descriptor")
   (chart-element :accessor chart-element :initarg :chart-element
		  :initform nil)
   (reason :accessor reason :initarg :reason :initform "not given"))
  (:report (lambda (condition stream)
	     (format stream "Invalid state descriptor or unknown state: ~a~%~a"
		     (descriptor condition) (reason condition)))))

(define-condition couldnt-join-state-names (error)
  ((state-a :accessor state-a :initarg :state-a :initform nil)
   (state-b :accessor state-b :initarg :state-b :initform nil)
   (reason :accessor reason :initarg :reason :initform "not given"))
  (:report (lambda (condition stream)
	     (format stream "Couldn't join states: ~a and ~a ~%~a"
		     (state-a condition) (state-b condition)
		     (reason condition)))))

(define-condition couldnt-determine-final-state (error)
  ((initial-state :accessor initial-state :initarg :initial-state :initform nil)
   (event :accessor event :initarg :event :initform "not given")
   (given-condition :accessor given-condition :initarg
		    :given-condition :initform nil))
  (:report (lambda (condition stream)
	     (format stream "Couldn't determine final state for event: ~a with initial state: ~a ~%~a"
		     (event condition) (initial-state condition)
		     (if (given-condition condition)
			 (given-condition condition)
			 "Unknown reason")))))

(define-condition couldnt-find-default-state (error)
  ((cluster :accessor cluster :initarg :cluster :initform "unknown")
   (default-state :accessor default-state :initarg :default-state :initform "unknown"))
  (:report (lambda (c stream)
	     (format stream "Couldn't find default state: ~a for cluster: ~a"
		     (default-state c) (cluster c)))))

(define-condition invalid-chart-syntax (program-error)
  ((message :accessor message :initarg :message :initform nil)
   (offending-code :accessor offending-code :initarg :offending-code :initform '()))
  (:report (lambda (condition stream)
	     (format stream "Invalid chart syntax given. ~%")
	     (if (message condition) (format stream "~a" (message condition)))
	     (if (offending-code condition)
		 (format stream "~tOffending code: ~a"
			 (offending-code condition)))))         	                    
  (:documentation "doc"))

(define-condition invalid-transition (program-error)
  ((initial-state-name :accessor initial-state-name
		       :initarg :initial-state-name :initform "")
   (event-name :accessor event-name :initarg :event-name :initform ""))
  (:report (lambda (c stream)
	     (format stream "Couldn't compute transition for state: ~a with event: ~a"
		     (initial-state-name c)
		     (event-name c))))         	                    
  (:documentation "doc"))







