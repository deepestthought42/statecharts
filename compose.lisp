(in-package #:statecharts)

#|

(defun %register-transition (statechart initial-key final-key transition-object)
  (let+ (((&slots transitions) statechart)
	 (key (append initial-key '(:->) final-key))
	 (existing (gethash key transitions)))
    (if existing (error 'transition-exists :key key))
    (setf (gethash key transitions) transition-object))
  transition-object)


(defun %register-state (statechart superstate state-name state-object)
  (let+ (((&slots states) statechart)
	 (key (%dereference-key superstate state-name))
	 (existing (gethash key states)))
    (if existing (error 'state-exists :key key))
    (setf (gethash key states) state-object)
    state-object))

(defun %register-event (statechart event-name transition-object)
  (let+ (((&slots events) statechart)
	 (event (alexandria:if-let (existing (gethash event-name events))
		  existing
		  (setf (gethash event-name events)
			(make-instance 'event :name event-name)))))
    (push transition-object (registered-transitions event))))

|#
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







