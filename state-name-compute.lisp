(in-package #:statecharts.state-key)

;; the dsl to address states is simple:
;; - "A" references the state with name "A" in the current superstate
;;  
;; - ("H" "A") references the the substate "A" of state "H" within
;;   the current superstate
;; - ("H" ("A" "B") ("C" "D")) references  the substate(s) of
;;   the orthogonal cluster "H"; since a substate of an orthogonal cluster is always 
;;   active, just the name of the substate (e.g. "A") is nonsensical


;; fixmee: should have done this with pattern matching ... 

(defun from-description (state-description dsl-element &optional super-state)
  (labels ((throw-invalid (reason &rest args)
	     (error 'sc.cond::invalid-state-descriptor
		    :descriptor state-description
		    :reason (apply #'format nil reason args)))
	   (state-p (obj) (typep obj 'sc.dsl::state))
	   (find-element (state elements)
	     (alexandria:if-let (sub-s (find state (remove-if-not #'state-p elements)
					     :key #'sc.dsl::name
					     :test #'string=))
	       sub-s (throw-invalid "Could not find state with name: ~a" state)))
	   (make-and (state %element states-description)
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
						    sub-state (find-element s-name (sc.dsl::elements %element))))))))
				  ;; direct sub-state
				  ((stringp (first states-description))
				   (list (%make-state-name
					  states-description
					  (find-element (first states-description)
							(sc.dsl::elements %element)))))))
		    (no-duplicates (remove-duplicates sub-states :test #'string= :key #'name)))
	       (if (not (= (length sub-states)
			   (length no-duplicates)))
		   (throw-invalid "Found duplicate substate definitions for: ~a"
				  (mapcar #'name (set-difference sub-states no-duplicates))))
	       (make-instance 'and-state :name state :sub-states sub-states)))
	   (%make-state-name (%state-description %element)
	     (let+ (((state &rest rest) %state-description))
	       (cond
		 ((not %state-description) nil)
		 ((not (and (stringp state)
			    (string= state (sc.dsl::name %element))))
		  (throw-invalid "Unknown or invalid state name: ~a" state))
		 ;; orthogonal cluster
		 ((and (typep %element 'sc.dsl::orthogonal))
		  (make-and state %element rest))
		 ;; cluster state
		 ((and (typep %element 'sc.dsl::cluster)
		       (not (typep %element 'sc.dsl::orthogonal)))
		  (make-instance 'or-state
				 :name state
				 :sub-state
				 (cond
				   ((not (first rest)) nil)
				   ((stringp (first rest))
				    (%make-state-name rest (find-element (first rest)
									 (sc.dsl::elements %element))))
				   (t (throw-invalid "Invalid state syntax: ~a" (first rest))))))
		 ;; leaf state
		 ((and (typep %element 'sc.dsl::state)
		       (not (typep %element 'sc.dsl::cluster)))
		  (make-instance 'state :name state))
		 ;;
		 (t (throw-invalid "Couldn't parse key: ~a" %state-description))))))
    (cond
      ((and (listp state-description)
	    (equal (first state-description) :/))
       (%make-state-name (rest state-description) dsl-element))
      ((and super-state (listp state-description))
       (%make-state-name (append super-state state-description) dsl-element))
      ((listp state-description)
       (%make-state-name (append super-state state-description) dsl-element))
      (t
       (%make-state-name (append super-state (list state-description)) dsl-element)))))


;;; copy state-names
(defgeneric copy (state))

(defmethod copy ((sn state))
  (make-instance 'state :name (name sn)))

(defmethod copy ((sn or-state))
  (make-instance 'or-state
		 :name (name sn)
		 :sub-state
		 (if (sub-state sn)
		     (copy (sub-state sn)))))

(defmethod copy ((sn and-state))
  (make-instance 'and-state
		 :name (name sn)
		 :sub-states
		 (mapcar #'copy (sub-states sn))))

;;; joining ops

(defun throw-couldnt-join-state-names (a b reason &rest args)
  (error 'sc.cond::couldnt-join-state-names
	 :state-a a
	 :state-b b
	 :reason (format nil reason args)))

(labels ((throw-couldnt-diff (a b)
	   (throw-couldnt-join-state-names a b "States are of different type.")))
  (defgeneric join (a b)
    (:method ((a state) (b t))
      (throw-couldnt-diff a b))
    (:method ((a t) (b state))
      (throw-couldnt-diff a b))
    (:method ((a or-state) (b t))
      (throw-couldnt-diff a b))
    (:method ((a t) (b or-state))
      (throw-couldnt-diff a b))
    (:method ((a and-state) (b t))
      (throw-couldnt-diff a b))
    (:method ((a t) (b and-state))
      (throw-couldnt-diff a b))))


(defmethod join ((a state) (b state))
  (cond
    ((string= (name a) (name b))
     (make-instance 'state :name (name a)))
    (t (throw-couldnt-join-state-names a b "States do no not match."))))

(defmethod join ((a or-state) (b or-state))
  (cond
    ((string= (name a) (name b))
     (make-instance 'or-state
		    :name (name a)
		    :sub-state (join (sub-state a) (sub-state b))))
    (t (throw-couldnt-join-state-names a b "States do no not match."))))

(defmethod join ((a and-state) (b and-state))
  (labels ((app-sns () 
	     (let+ ((subs-a (sub-states a))
		    (subs-b (sub-states b))
		    (unionized (union subs-a subs-b :key #'name :test #'string=)))
	       (iter
		 (for sn in unionized)
		 (for sa = (find (name sn) subs-a :key #'name :test #'string=))
		 (for sb = (find (name sn) subs-b :key #'name :test #'string=))
		 (cond
		   ((and sa sb) (collect (join sa sb)))
		   (sa (collect sa))
		   (sb (collect sb)))))))
    (cond
      ((string= (name a) (name b))
       (make-instance 'and-state :name (name a) :sub-states (app-sns)))
      (t (throw-couldnt-join-state-names a b "States do no not match.")))))


;;; intersection ops
(defgeneric intersect-state-names (a b)
  (:method (a b) (make-instance 'state :name (name a))))



(defmethod intersect-state-names ((a state) (b state))
  (cond
    ((string= (name a) (name b)) '())
    (t (make-instance 'state :name (name a)))))

(defmethod intersect-state-names ((a or-state) (b or-state))
  (cond
    ((string= (name a) (name b))
     (alexandria:if-let (substate (intersect-state-names (sub-state a) (sub-state b)))
       (make-instance 'or-state
		      :name (name a)
		      :sub-state substate)))
    (t '())))

(defmethod intersect-state-names ((a and-state) (b and-state))
  (cond
    ((string= (name a) (name b))
     (let (substates-a substates-b)
       (iter
	 (for sub-a in (sub-states a))
	 (for sub-b = (find (name sub-a) (sub-states b)
			    :key #'name :test #'string=))
	 (when (and sub-a sub-b)
	   (push sub-a substates-a)
	   (push sub-b substates-b)))
       (alexandria:if-let (subs (remove-if #'not
					   (mapcar #'intersect-state-names
						   substates-a substates-b)))
	 (make-instance 'and-state :name (name a) :sub-states subs))))
    (t '())))


;;; set-difference-state-names

(defgeneric difference (a b &key accept-unspecified-substate)
  (:method (a b &key accept-unspecified-substate)
    (declare (ignore b accept-unspecified-substate))
    (copy a))
  (:documentation "Return the part (and/or) of key A that is not present in key
B.  When ACCEPT-UNSPECIFIED-SUBSTATE is true, unspecified substates in a cluster
are ignored, such that: (difference (a b) (a)) -> NIL."))

(defun %same-name (a b)
  (string= (name a) (name b)))


(defmethod difference ((a state) (b state) &key accept-unspecified-substate)
  (declare (ignore accept-unspecified-substate))
  (cond
    ((%same-name a b) '())
    (t (copy a))))

(defmethod difference ((a or-state) (b state) &key accept-unspecified-substate)
  (if (and (%same-name a b) accept-unspecified-substate)
      '()
      (copy a)))

(defmethod difference ((a or-state) (b or-state) &key accept-unspecified-substate)
  (cond
    ((and (%same-name a b) (sub-state a) (not (sub-state b)))
     (if accept-unspecified-substate
	 '() (copy a)))
    ((and (%same-name a b) (not (sub-state a)))
     '())
    ((%same-name a b)
     (let ((diff (difference (sub-state a) (sub-state b)
			     :accept-unspecified-substate accept-unspecified-substate)))
       (if diff
	   (make-instance 'or-state
			  :name (name a)
			  :sub-state (copy diff)))))
    (t (copy a))))

(defmethod difference ((a and-state) (b and-state) &key accept-unspecified-substate)
  (cond
    ((string= (name a) (name b))
     (let (diff)
       (iter
	 (for sub-a in (sub-states a))
	 (for sub-b = (find (name sub-a) (sub-states b)
			    :key #'name :test #'string=))
	 (cond
	   ((not sub-b) (push (copy sub-a) diff))
	   (t (push (difference sub-a sub-b
				:accept-unspecified-substate
				accept-unspecified-substate)
		    diff))))
       (alexandria:if-let (diff (remove-if #'not diff))
	 (make-instance 'and-state :name (name a) :sub-states diff))))
    (t (copy a))))


;;; compare state names

(defgeneric state= (a b)
  (:documentation "Returns true for objects A, B of type STATE-NAME that are same."))

(defmethod state= ((a state) (b t)) nil)
(defmethod state= ((a t) (b state)) nil)

(defmethod state= ((a state) (b state))
  (string= (name a) (name b)))

(defmethod state= ((a or-state) (b or-state))
  (and (string= (name a) (name b))
       (state= (sub-state a) (sub-state b))))

(defmethod state= ((a and-state) (b and-state))
  (and (string= (name a) (name b))
       (= (length (sub-states a))
	  (length (sub-states b)))
       (iter
	 (for sa in (sort (sub-states a) #'string< :key #'name))
	 (for sb in (sort (sub-states b) #'string< :key #'name))
	 (if (not (state= sa sb))
	     (return nil))
	 (finally (return t)))))


;;; find dsl-object for state
(defgeneric find-dsl-object (state dsl-object)
  (:documentation "Given a state in STATE-NAME, will return the branch of DSL-OBJECT
  that corresponds to STATE-NAME. Returns NIL if it cannot find a dsl-object that
  correpsonds to STATE-NAME.")
  (:method ((state t) (dsl-object t)) nil))


(defmethod find-dsl-object ((state state) (dsl-object sc.dsl::state))
  (when (string= (name state) (sc.dsl::name dsl-object)) dsl-object))

(defmethod find-dsl-object ((state or-state) (dsl-object sc.dsl::cluster))
  (when (string= (name state) (sc.dsl::name dsl-object))
    (let ((states (remove-if-not #'(lambda (e) (typep e 'sc.dsl::state))
				 (sc.dsl::elements dsl-object))))
      ;; empty sub-state -> return dsl-object
      (if (not (sub-state state))
	  dsl-object
	  (iter
	    (for s in states)
	    (for o = (find-dsl-object (sub-state state) s))
	    (if o (return o)))))))


(defmethod find-dsl-object ((state and-state) (dsl-object sc.dsl::orthogonal))
  (when (string= (name state) (sc.dsl::name dsl-object))
    (let ((states (remove-if-not #'(lambda (e) (typep e 'sc.dsl::state))
				 (sc.dsl::elements dsl-object))))
      ;; empty sub-state -> return dsl-object
      (cond
	((not (sub-states state)) dsl-object)
	((= 1 (length (sub-states state)))
	 (iter
	   (for s in states)
	   (for o = (find-dsl-object (first (sub-states state)) s))
	   (if o (return o))))
	;; maybe throw error here ?
	(t nil)))))

;;; from-chart-state state-name from s

(defgeneric from-chart-state (s))

(defmethod from-chart-state ((s sc.chart::s))
  (make-instance 'sc.key::state :name (sc.chart::name s)))

(defmethod from-chart-state ((s sc.chart::s-xor))
  (make-instance 'sc.key::or-state
		 :name (sc.chart::name s)
		 :sub-state (from-chart-state (sc.chart::sub-state s))))

(defmethod from-chart-state ((s sc.chart::s-and))
  (make-instance 'sc.key::and-state
		 :name (sc.chart::name s)
		 :sub-states (mapcar #'from-chart-state (sc.chart::sub-states s))))
