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







