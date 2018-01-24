(in-package :statecharts)


;;; need to have a better way to represent states internally than a conses


(defclass state-name ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))))

(defclass or-state-name (state-name)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))))

(defclass and-state-name (state-name)
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



(defun make-state-name (state-description chart-element &optional super-state)
  (labels ((throw-invalid (reason &rest args)
	     (error 'invalid-state-descriptor :descriptor state-description
					      :chart-element chart-element
					      :reason (apply #'format nil reason args)))
	   (find-element (state-name elements)
	     (alexandria:if-let (sub-s (find state-name (remove-if-not #'state-p elements)
					     :key #'name
					     :test #'string=))
	       sub-s (throw-invalid "Could not find state with name: ~a" state-name)))
	   (make-and (state-name %element states-description)
	     (let+ ((sub-states (cond
				  ;; multiple sub-states
				  ((listp (first states-description))
				   (iter
				     (for sub-state in states-description)
				     (for s-name = (first sub-state))
				     (cond
				       ((not (stringp s-name))
					(throw-invalid "State name not a string: ~a" s-name))
				       (t (collect (%make-state-name
						    sub-state (find-element s-name (elements %element))))))))
				  ;; direct sub-state
				  ((stringp (first states-description))
				   (list (%make-state-name
					  states-description
					  (find-element (first states-description)
							(elements %element)))))))
		    (no-duplicates (remove-duplicates sub-states :test #'string= :key #'name)))
	       (if (not (= (length sub-states)
			   (length no-duplicates)))
		   (throw-invalid "Found duplicate substate definitions for: ~a"
				  (mapcar #'name (set-difference sub-states no-duplicates))))
	       (make-instance 'and-state-name :name state-name :sub-states sub-states)))
	   (%make-state-name (%state-description %element)
	     (let+ (((state-name &rest rest) %state-description))
	       (cond
		 ((not %state-description) nil)
		 ((not (and (stringp state-name)
			    (string= state-name (name %element))))
		  (throw-invalid "Unknown or invalid state name: ~a" state-name))
		 ;; orthogonal cluster
		 ((and (typep %element 'orthogonal))
		  (make-and state-name %element rest))
		 ;; cluster state
		 ((and (typep %element 'cluster)
		       (not (typep %element 'orthogonal)))
		  (make-instance 'or-state-name :name state-name
						:sub-state
						(cond
						  ((not (first rest)) nil)
						  ((stringp (first rest))
						   (%make-state-name rest (find-element (first rest)
											(elements %element))))
						  (t (throw-invalid "Invalid state syntax: ~a" (first rest)))))) 
		 ;; leaf state
		 ((and (typep %element 'state)
		       (not (typep %element 'cluster)))
		  (make-instance 'state-name :name state-name))
		 ;; 
		 (t (throw-invalid "Couldn't parse key: ~a" %state-description))))))
    (cond
      ((and (listp state-description)
	    (equal (first state-description) :/))
       (%make-state-name (rest state-description) chart-element))
      ((and super-state (listp state-description))
       (%make-state-name (append super-state state-description) chart-element))
      ((listp state-description)
       (%make-state-name (append super-state state-description) chart-element))
      (t
       (%make-state-name (append super-state (list state-description)) chart-element)))))






