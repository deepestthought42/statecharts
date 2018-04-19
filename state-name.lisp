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
		  (make-instance 'or-state-name
				 :name state-name
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


;;; copy state-names
(defgeneric copy-state-name (state-name))

(defmethod copy-state-name ((sn state-name))
  (make-instance 'state-name :name (name sn)))

(defmethod copy-state-name ((sn or-state-name))
  (make-instance 'or-state-name
		 :name (name sn)
		 :sub-state
		 (copy-state-name (sub-state sn))))

(defmethod copy-state-name ((sn and-state-name))
  (make-instance 'and-state-name
		 :name (name sn)
		 :sub-states
		 (mapcar #'copy-state-name (sub-states sn))))

;;; joining ops

(defun throw-couldnt-join-state-names (a b reason &rest args)
  (error 'couldnt-join-state-names
	 :state-a a
	 :state-b b
	 :reason (format nil reason args)))

(labels ((throw-couldnt-diff (a b)
	   (throw-couldnt-join-state-names a b "States are of different type.")))
  (defgeneric join-state-names (a b)
    (:method ((a state-name) (b t))
      (throw-couldnt-diff a b))
    (:method ((a t) (b state-name))
      (throw-couldnt-diff a b))
    (:method ((a or-state-name) (b t))
      (throw-couldnt-diff a b))
    (:method ((a t) (b or-state-name))
      (throw-couldnt-diff a b))
    (:method ((a and-state-name) (b t))
      (throw-couldnt-diff a b))
    (:method ((a t) (b and-state-name))
      (throw-couldnt-diff a b))))


(defmethod join-state-names ((a state-name) (b state-name))
  (cond
    ((string= (name a) (name b))
     (make-instance 'state-name :name (name a)))
    (t (throw-couldnt-join-state-names a b "States do no not match."))))

(defmethod join-state-names ((a or-state-name) (b or-state-name))
  (cond
    ((string= (name a) (name b))
     (make-instance 'or-state-name
		    :name (name a)
		    :sub-state (join-state-names (sub-state a) (sub-state b))))
    (t (throw-couldnt-join-state-names a b "States do no not match."))))

(defmethod join-state-names ((a and-state-name) (b and-state-name))
  (labels ((app-sns ()
	     (let+ ((unionized (union (sub-states a) (sub-states b) :test #'state-name=))
		    (no-duplicates (remove-duplicates unionized :key #'name :test #'string=)))
	       (if (not (= (length unionized)
			   (length no-duplicates)))
		   (throw-couldnt-join-state-names a b "Duplicate sub states")
		   (mapcar #'copy-state-name no-duplicates)))))
    (cond
      ((string= (name a) (name b))
       (make-instance 'and-state-name
		      :name (name a)
		      :sub-states
		      (app-sns)))
      (t (throw-couldnt-join-state-names a b "States do no not match.")))))


;;; compare state names

(defgeneric state-name= (a b))

(defmethod state-name= ((a state-name) (b t)) nil)
(defmethod state-name= ((a t) (b state-name)) nil)

(defmethod state-name= ((a state-name) (b state-name))
  (string= (name a) (name b)))

(defmethod state-name= ((a or-state-name) (b or-state-name))
  (and (string= (name a) (name b))
       (state-name= (sub-state a) (sub-state b))))

(defmethod state-name= ((a and-state-name) (b and-state-name))
  (and (string= (name a) (name b))
       (= (length (sub-states a))
	  (length (sub-states b)))
       (iter
	 (for sa in (sort (sub-states a) #'string< :key #'name))
	 (for sb in (sort (sub-states b) #'string< :key #'name))
	 (if (not (state-name= sa sb))
	     (return nil))
	 (finally (return t)))))

;;; printing state-name

(defmethod print-object ((obj state-name) stream)
  (print-unreadable-object (obj stream)
    (format stream "name: ")
    (print-state-name obj stream)))

(defmethod print-state-name ((obj state-name) stream)
  (format stream "~a" (name obj)))

(defmethod print-state-name ((obj or-state-name) stream)
  (format stream "(~a" (name obj))
  (when (sub-state obj)
    (format stream " ")
    (print-state-name (sub-state obj) stream))
  (format stream ")"))


(defmethod print-state-name ((obj and-state-name) stream)
  (format stream "(~a" (name obj))
  (let ((sub-states (sub-states obj)))
    (when sub-states
      (format stream " ")
      (print-state-name (car sub-states) stream)
      (map nil #'(lambda (s)
		   (format stream "∧")
		   (print-state-name s stream))
	   (cdr sub-states))))
  (format stream ")"))
