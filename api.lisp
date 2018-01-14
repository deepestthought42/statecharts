(in-package #:statecharts)


(define-condition state-exists (error)
  ((key :accessor key :initarg :key :initform "unspecified key")))

(define-condition transition-exists (error)
  ((key :accessor key :initarg :key :initform "unspecified transition")))

(define-condition invalid-state-descriptor (error)
  ((descriptor :accessor descriptor :initarg :descriptor
	       :initform "unspecified descriptor"))
    (:report (lambda (condition stream)
	       (format stream "Invalid state descriptor: ~a" (descriptor condition)))))


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
  ((initial-key :accessor initial-key :initarg :initial-key :initform "")
   (event-key :accessor event-key :initarg :event-key :initform ""))
  (:report (lambda (c stream)
	     (format stream "Couldn't compute transition for state: ~a with event: ~a"
		     (initial-key c)
		     (transition-key c))))         	                    
  (:documentation "doc"))







