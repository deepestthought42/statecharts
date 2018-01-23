(in-package :statecharts)


;;; need to have a better way to represent states internally than a conses


(defclass key ()
  ((state-name :initarg :state-name :accessor state-name 
	       :initform (error "Must initialize state-name."))))

(defclass or-key (key)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))))

(defclass and-key (key)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))


;; the dsl to address states is simple:
;; - "A" references the state with name "A" in the current superstate
;;  
;; - ("H" "A") references the the substate(s) "A" of state "H" within
;;   the current superstate
;; - ("H" ("A" "B") ("C" "D")) references  the substate(s) of
;;   the orthogonal cluster "H"; each element following :and is treated
;;   as a substate; since a substate of an orthogonal cluster is always 
;;   active, just the name of the substate (e.g. "A") is nonsensical





(defun make-key (key-description chart-element)
  (labels ((throw-invalid (reason &rest args)
	     (error 'invalid-state-descriptor :descriptor key-description
					      :chart-element chart-element
					      :reason (apply #'format nil reason args)))
	   (find-element (key elements)
	     (alexandria:if-let (sub-s (find key (remove-if-not #'state-p elements)
					     :key #'name
					     :test #'string=))
	       sub-s (throw-invalid "Could not find state with name: ~a" key)))
	   (make-and (key %element states-description)
	     (let+ ((sub-states (iter
				 (for sub-state in states-description)
				 (if (not (listp sub-state))
				     (throw-invalid "Incorrect syntax for substate: ~a" sub-state))
				 (for key = (first sub-state))
				 (if (not (stringp key))
				     (throw-invalid "State name not a string: ~a" key))
				 (collect (%make-key sub-state
						     (find-element key (elements %element))))))
		   (no-duplicates (remove-duplicates sub-states :test #'string= :key #'state-name)))
	       (if (not (= (length sub-states)
			   (length no-duplicates)))
		   (throw-invalid "Found duplicate substate definitions for: ~a"
				  (mapcar #'state-name (set-difference sub-states no-duplicates))))
	       (make-instance 'and-key :state-name key :sub-states sub-states)))
	   (%make-key (%key-description %element)
	     (let+ (((key &rest rest) %key-description))
	       (cond
		 ((not %key-description) nil)
		 ((not (and (stringp key)
			    (string= key (name %element))))
		  (throw-invalid "Unknown or invalid state name: ~a" key))
		 ;; orthogonal cluster
		 ((and (typep %element 'orthogonal))
		  (make-and key %element rest))
		 ;; cluster state
		 ((and (typep %element 'cluster)
		       (not (typep %element 'orthogonal)))
		  (make-instance 'or-key :state-name key
					 :sub-state
					 (cond
					   ((not (first rest)) nil)
					   ((stringp (first rest))
					    (make-key rest (find-element (first rest)
									 (elements %element))))
					   (t (throw-invalid "Invalid state syntax: ~a" (first rest)))))) 
		 ;; leaf state
		 ((and (typep %element 'state)
		       (not (typep %element 'cluster)))
		  (make-instance 'key :state-name key))
		 ;; 
		 (t (throw-invalid "Couldn't parse key: ~a" %key-description))))))
    (%make-key key-description chart-element)))






