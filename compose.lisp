(in-package #:statecharts)

;;; definitions for objects used during parsing / creation of fsm

(defun state-p (obj)
  (typep obj 'state))


(defclass s ()
  ((key :initarg :key :accessor key 
	:initform (error "Must initialize key."))))

(defclass s-xor (s)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))))

(defclass s-and (s)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))


(defclass tr ()
  ((initial-key :initarg :initial-key :accessor initial-key 
		:initform (error "Must initialize initial-key."))
   (final-key :initarg :final-key :accessor final-key 
	      :initform (error "Must initialize final-key."))
   (event-key :initarg :event-key :accessor event-key 
	      :initform (error "Must initialize event-key."))
   (guard :initarg :guard :accessor guard 
	  :initform (error "Must initialize guard."))))

;;; parse statecharts

(defgeneric compute-substates (s))

(defmethod compute-substates ((s t)) '())

(defmethod compute-substates ((s state))
  (list (make-instance 's
		       :key (name s))))

(defmethod compute-substates ((cluster cluster))
  (let+ (((&slots name elements transitions) cluster))
    (iter outer
      (for e in elements)
      (for sub-states = (compute-substates e))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance 's-xor :key name :sub-state s)))))))

(defmethod compute-substates ((ortho orthogonal))
  (let+ (((&slots name elements) ortho)
	 (lst-of-lst-of-substates
	  (remove-if #'not
		     (mapcar #'(lambda (s) (compute-substates s))
			     elements))))
    (labels ((combine-elements (super-lst) 
	       (cond
		 ((not (cdr super-lst))
		  (mapcar #'(lambda (s)
			      (make-instance 's-and :sub-states (list s)
					     :key name))
			  (car super-lst)))
		 (t
		  (let ((states-1 (car super-lst))
			(states-n (combine-elements (rest super-lst))))
		    (iter
		      (for s-1 in states-1)
		      (appending
		       (mapcar #'(lambda (s-n)
				   (make-instance 's-and :key name
							 :sub-states
							 (append (list s-1)
								 (sub-states s-n))))
			       states-n))))))))
      (combine-elements lst-of-lst-of-substates))))


;;; transitions

(defun %dereference-key (superstate key)
  (labels ((syntactically-valid-descriptor (desc)
	     (cond
	       ((stringp desc) t)
	       ((symbolp desc) t)
	       ((and (listp desc)
		     (equal :and (first desc)))
		(mapcar #'syntactically-valid-descriptor (cdr desc)))
	       ((and (listp desc)
		     (stringp (first desc)))
		(mapcar #'syntactically-valid-descriptor desc))
	       (t (error 'invalid-state-descriptor :descriptor desc)))))
    (cond
      ;; (/ "A" "B" "C") references with respect to the root of the tree:
      ;; -> "C" within "B" within "A"
      ((and (listp key) (equal '/ (first key))
	    (syntactically-valid-descriptor (cdr key)))
       (cdr key))
      ;; ("A" "B") references within the current super state:
      ;; "B" within "A" within the current superstate
      ((and (listp key)
	    (syntactically-valid-descriptor key)
	    (or (equal nil superstate)
		(syntactically-valid-descriptor superstate)))
       (append superstate key))
      ;; "A" references "A" within the current superstate
      ((and (not (listp key))
	    (syntactically-valid-descriptor key))
       (append superstate (list key)))
      (t (error 'invalid-state-descriptor
		:descriptor key)))))


(defmethod compute-transitions ((s t) super-state) '())

(defun %make-transitions (elements super-state)
  (iter
    (for el in elements)
    (when (typep el 'transition)
      (collect
	  (make-instance 'tr
			 :event-key (event el)
			 :guard (guard el)
			 :initial-key
			 (%dereference-key super-state
					   (initial-state el))
			 :final-key
			 (%dereference-key super-state
					   (final-state el)))))))


(defmethod compute-transitions ((s cluster) super-state)
  (let+ (((&slots name elements) s)
	 (super-state (append super-state (list name)))
	 (transitions-for-sub-states
	  (iter
	    (for el in elements)
	    (appending (compute-transitions el super-state)))))
    (append transitions-for-sub-states
	    (%make-transitions elements super-state))))



;;; printing

(defmethod print-object ((obj tr) stream)
  (print-unreadable-object (obj stream)
    (format stream "~a --|~a|--> ~a"
	    (initial-key obj)
	    (event-key obj)
	    (final-key obj))))


(defmethod print-object ((obj s) stream)
  (print-unreadable-object (obj stream)
    (print-s obj stream)))

(defmethod print-s ((obj s) stream)
  (format stream "~a" (key obj)))

(defmethod print-s ((obj s-xor) stream)
  (format stream "(~a " (key obj))
  (print-s (sub-state obj) stream)
  (format stream ")"))


(defmethod print-s ((obj s-and) stream)
  (format stream "(~a " (key obj))
  (let ((sub-states (sub-states obj)))
    (when sub-states
      (print-s (car sub-states) stream)
      (map nil #'(lambda (s)
		   (format stream "∧")
		   (print-s s stream))
	   (cdr sub-states))))
  (format stream ")"))







