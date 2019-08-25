(defpackage #:statecharts.chart
  (:use #:cl #:iterate #:let-plus #:sc.cond)
  (:import-from #:statecharts.dsl
		transition-clause
		otherwise-clause
		guard-clause)
  (:nicknames #:sc.chart))

#+nil
(declaim (optimize (debug 3) (speed 0) (space 0)))

(in-package #:sc.chart)


(defclass s ()
  ((name :initarg :name :accessor name :initform (error "Must initialize name."))
   (defining-state :initarg :defining-state :accessor defining-state :initform nil)
   (on-entry :accessor on-entry :initarg :on-entry :initform '())
   (on-reentry :accessor on-reentry :initarg :on-reentry :initform '())
   (on-exit :accessor on-exit :initarg :on-exit :initform '())
   (no-of-substates :accessor no-of-substates :initarg :no-of-substates :initform 0)
   (id :accessor id :initarg :id :initform (error "Need to initialize ID."))))



(defclass s-xor (s)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))))


(defclass history-s-xor (s-xor) ())


(defclass s-and (s)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))


(defmethod initialize-instance :after ((obj s-and) &key)
  (setf (sub-states obj) (sort (sub-states obj) #'string< :key #'name)))

(defgeneric copy-state (s &key))

(sc.utils::define-copy-object-method (s copy-state)
  name defining-state on-entry on-reentry on-exit id)

(sc.utils::define-copy-object-method (s-xor copy-state)
  name defining-state on-entry on-reentry on-exit sub-state default-state id)

(sc.utils::define-copy-object-method (s-and copy-state)
  name defining-state on-entry on-reentry on-exit sub-states id)


(defgeneric get-leaf (s)
  (:method ((s s)) s)
  (:method ((s s-xor))
    (if (not (sub-state s))
	s (get-leaf (sub-state s))))
  (:method ((s s-and))
    (cond
      ((not (sub-states s)) s)
      ((= (length (sub-states s)) 1)
       (get-leaf (first (sub-states s))))
      (t (error "Cannot determine leaf of s-and with more than one substate.")))))




(defclass transition ()
  ((initial-state-name :initarg :initial-state-name :accessor initial-state-name 
		       :initform (error "Must initialize initial-state-name."))
   (event-name :initarg :event-name :accessor event-name 
	       :initform (error "Must initialize event-state-name."))
   (clauses :accessor clauses :initarg :clauses
	    :initform (error "Need to initialize CLAUSES."))))



;;; printing

(defmethod print-object ((obj transition) stream)
  (print-unreadable-object (obj stream)
    (sc.utils::%print-object (initial-state-name obj) stream)
    (format stream " --|~a|--> " (event-name obj))
    (cond
      ((and (> (length (clauses obj)) 1))
       (format stream "[cond")
       (iter
	 (for g in (clauses obj))
	 (if-first-time nil (format stream " | "))
	 (sc.utils::%print-object (sc.dsl::final-state g) stream))
       (format stream " ]"))
      (t (sc.utils::%print-object (sc.dsl::final-state
			     (first (clauses obj))) stream)))))


(defmethod print-object ((obj s) stream)
  (print-unreadable-object (obj stream)
    (print-s obj stream)))

(defmethod print-s ((obj s) stream)
  (format stream "(~a)" (name obj)))

(defmethod print-s ((obj s-xor) stream)
  (format stream "(~a " (name obj))
  (print-s (sub-state obj) stream)
  (format stream ")"))


(defmethod print-s ((obj s-and) stream)
  (format stream "(~a " (name obj))
  (let ((sub-states (sub-states obj)))
    (when sub-states
      (print-s (car sub-states) stream)
      (map nil #'(lambda (s)
	   (format stream "∧")
		   (print-s s stream))
	   (cdr sub-states))))
  (format stream ")"))


;;; state to name comparison

(defgeneric state=state-name (state state-name))

(defmethod  state=state-name ((state t)
			      (state-name t))
  nil)

(defmethod state=state-name ((state s)
			     (state-name sc.key::state))
  (eq (sc.key::name state-name)
	   (name state)))


(defmethod state=state-name ((state s-xor)
			     (state-name sc.key::or-state))
  (eq (sc.key::name state-name)
	   (name state)))

(defmethod state=state-name ((state s-and)
			     (state-name sc.key::and-state))
  (eq (sc.key::name state-name)
	   (name state)))



(defgeneric state-described-by-name (s state-name))


(defmethod state-described-by-name ((s s) state-name)
  (state=state-name s state-name))

(defmethod state-described-by-name ((s s-xor) state-name)
  (cond
    ;; name and state-name have to match as well as the type of name
    ((not (state=state-name s state-name)) nil)
    ;; the name matches but doesn't specify any sub-states -> match
    ((not (sc.key::sub-state state-name)) t)
    ;; the name still matches and specifies sub-states -> test substate
    (t (state-described-by-name (sub-state s) (sc.key::sub-state state-name)))))

(defmethod state-described-by-name ((s s-and) state-name)
  (labels ((is-sub-state (k)
	     (iter
	       (for sub-s in (sub-states s))
	       (if (state-described-by-name sub-s k)
		   (return t)))))
    (cond
      ;; name and state-name have to match as well as the type of state-name
      ((not (state=state-name s state-name)) nil)
      ;; the state-name doesn't specify any substates, so all good
      ((not (sc.key::sub-states state-name)) t)
      ;; name matches and we have substates, so compare them one be one
      (t (reduce #'(lambda (a b) (and a b))
		 (sc.key::sub-states state-name)
		 :key #'is-sub-state)))))


;; remove substates not explicitly speficied in state-name
(defgeneric remove-implicit-substates (state state-name)
  (:documentation "Remove substates from STATE not explicitly speficied in STATE-NAME."))

(defmethod remove-implicit-substates ((s (eql nil)) state-name)
  (error "Substate(s) given in state-name not found in state."))

(defun check-state-name (state state-name)
  (when (not (state=state-name state state-name))
    (error "State: ~a not described by state-name: ~a" state state-name)))

(defmethod remove-implicit-substates ((state s) state-name)
  (check-state-name state state-name)
  (copy-state state))

(defmethod remove-implicit-substates ((state s-xor) state-name)
  (check-state-name state state-name)
  (cond
    ;; the name matches but doesn't specify any sub-states -> return technically, this
    ;; doesn't make any sense but for determining which state is reentered it might be
    ;; useful
    ((not (sc.key::sub-state state-name)) (copy-state state :sub-state nil))
    ;; the name still matches and specifies sub-states -> test substate
    (t (copy-state state :sub-state
		   (remove-implicit-substates (sub-state state)
					      (sc.key::sub-state state-name))))))


(defmethod remove-implicit-substates ((state s-and) state-name)
  (check-state-name state state-name)
  (cond
    ;; the state-name doesn't specify any substates, so return nil
    ((not (sc.key::sub-states state-name)) (copy-state state :sub-states nil))
    ;; name matches and we have substates, so return only
    ;; the ones given in state-name
    (t
     (copy-state state :sub-states
		 (iter
		   (for sn in (sc.key::sub-states state-name))
		   (for s = (find (sc.key::name sn) (sub-states state)
				  :test #'eq :key #'name))
		   (when (not s) (error "Couldn't find substate with name: ~a" sn))
		   (for explicit = (remove-implicit-substates s sn))
		   (when explicit (collect explicit)))))))



;;; default states

(defmethod is-default-state ((s t)) nil)


(defmethod is-default-state ((s s)) t)


(defmethod is-default-state ((s s-xor))
  (let+ (((&slots default-state sub-state) s))
    (cond
      ((eq (name sub-state) default-state)
       (is-default-state sub-state))
      ;; no default state
      (t nil))))


(defmethod is-default-state ((s s-and))
  (reduce #'(lambda (a b)
	      (and a b))
	  (sub-states s)
	  :key #'is-default-state))

;;; difference between dsl-objects

(defgeneric difference (a b)
  (:method ((a t) (b t)) (values a b))
  (:documentation
   "Returns the states that are found in A but not in B as well as the states that are found
  in B but not A.

  => in-a-but-not-b*, in-b-but-not-a*"))

(defmethod difference ((a s) (b s))
  (if (= (sc.dsl::id (defining-state a))
	 (sc.dsl::id (defining-state b)))
      (values '() '())
      (values (list a) (list b))))

(defmethod difference ((a s-xor) (b s-xor))
  (if (= (sc.dsl::id (defining-state a))
	 (sc.dsl::id (defining-state b)))
      (difference (sub-state a) (sub-state b))
      (values (list a) (list b))))


(defmethod difference ((a s-and) (b s-and))
  (labels ((d-id (s) (sc.dsl::id (defining-state s))))
    (cond
      ((= (d-id a) (d-id b))
       (if (not (= (length (sub-states a))
		   (length (sub-states b))))
	   (error "This shouldn't be possible."))
       (iter
	 (for sub-a in (sub-states a))
	 (for sub-b in (sub-states b))
	 (for (values in-a in-b) = (difference sub-a sub-b))
	 (appending in-a into in-a-but-not-b)
	 (appending in-b into in-b-but-not-a)
	 (finally
	  (return
	    (values in-a-but-not-b in-b-but-not-a)))))
      (t (list a) (list b)))))



;;; these methods assume states S that correspond to the name given



(defgeneric is-partial-default-state (s state-name fixed))

(defmethod is-partial-default-state ((s s)
				     (state-name sc.key::state)
				     fixed)
  t)

(defmethod is-partial-default-state ((s s-xor)
				     (state-name sc.key::or-state)
				     fixed)
  (if (sc.key::sub-state state-name)
      (is-partial-default-state (sub-state s)
				(sc.key::sub-state state-name)
				(when fixed (sc.key::sub-state fixed)))
      (is-default-state s)))

(defmethod is-partial-default-state ((s s-and)
				     (state-name sc.key::and-state)
				     fixed)
  (labels ((%find-state-name (sub-s state-names)
	     (iter
	       (for state-name in state-names)
	       (if (state-described-by-name sub-s state-name)
		   (return state-name)))))
    (iter
      (for sub-s in (sub-states s))
      (for s-name = (%find-state-name sub-s (sc.key::sub-states state-name)))
      (for is-fixed = (when fixed
			    (%find-state-name sub-s (sc.key::sub-states fixed))))
      (cond
	((and s-name (not (is-partial-default-state sub-s s-name is-fixed))) (return nil))
	((and (not s-name) (not is-fixed) (not (is-default-state sub-s))) (return nil)))
      (finally (return t)))))


(defgeneric is-history-state (state))

(defmethod is-history-state ((state t)) nil)

(defmethod is-history-state ((state history-s-xor)) t)

(defmethod is-history-state ((s s-xor))
  (is-history-state (sub-state s)))

(defmethod is-history-state ((s s-and))
  (iter
    (for sub-s in (sub-states s))
    (when (is-history-state sub-s)
      (return t))
    (finally (return nil))))



(defun get-states-described-by-name (lst-of-states state-name)
  (remove-if-not #'(lambda (s) (state-described-by-name s state-name))
		 lst-of-states))


(defun resolve-final-state (possible-states state-name fixed)
  (let+ (;; in theory, these are more states than necessary as not all of
	 ;; history-states will have exits, but keeping track of this is
	 ;; bothersome; fixme: if this should be a performance issue, we might
	 ;; wanna fix it
	 (history-states (remove-if-not #'is-history-state possible-states))
	 (resolved-states
	  (remove-if-not #'(lambda (s)
			     (is-partial-default-state s state-name fixed))
			 possible-states))
	 (final-state resolved-states))
    (unless (= (length final-state) 1)
      (error "Implementation error resolving default state for: ~a" state-name))
    (values (first resolved-states) history-states)))


