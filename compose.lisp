(in-package #:statecharts)

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




(defmethod get-substates ((s t)) '())

(defmethod get-substates ((s state))
  (list (make-instance 's :key (name s))))

(defmethod get-substates ((cluster cluster))
  (let+ (((&slots name elements) cluster))
    (iter outer
      (for e in elements)
      (for sub-states = (get-substates e))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance 's-xor :key name :sub-state s)))))))




(defmethod get-substates ((ortho orthogonal))
  (let+ (((&slots name elements) ortho)
	 (lst-of-lst-of-substates
	  (remove-if #'not
		     (mapcar #'(lambda (s) (get-substates s))
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
  (labels ((valid-descriptor (desc)
	     (unless (stringp desc)
	       (error 'invalid-state-descriptor
		      :descriptor desc))))
    (cond
      ;; (/ "A" "B" "C") references with respect to the root of the tree:
      ;; -> "C" within "B" within "A"
      ((and (listp key) (equal '/ (first key))
	    (mapcar #'valid-descriptor (cdr key)))
       (cdr key))
      ;; ("A" "B") references within the current super state:
      ;; "B" within "A" within the current superstate
      ((and (listp key)
	    (mapcar #'valid-descriptor key)
	    (or (equal nil superstate)
		(mapcar #'valid-descriptor superstate)))
       (append superstate key))
      ;; "A" references "A" within the current superstate
      ((stringp key)
       (append superstate (list key)))
      (t (error 'invalid-state-descriptor
		:descriptor key)))))





(defmethod get-transitions ((tr transition)))



;;; printing

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







