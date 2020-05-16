(defpackage #:statecharts.chart
  (:use #:cl #:iterate #:let-plus #:sc.cond)
  (:import-from #:statecharts.dsl
		transition-clause
		otherwise-clause
		guard-clause)
  (:nicknames #:sc.chart))

#+nil (declaim (optimize (speed 3) (safety 1)))

(in-package #:sc.chart)


(defclass s ()
  ((name :initarg :name :accessor name :initform (error "Must initialize name."))
   (defining-state :initarg :defining-state :accessor defining-state :initform nil)
   (on-entry :accessor on-entry :initarg :on-entry :initform '())
   (on-reentry :accessor on-reentry :initarg :on-reentry :initform '())
   (on-exit :accessor on-exit :initarg :on-exit :initform '())
   (fsm-state :accessor fsm-state :initarg :fsm-state)
   (state-bit :accessor state-bit :initarg :state-bit
	      :initform 0)
   (default-state-bit :accessor default-state-bit :initarg :default-state-bit
		      :initform 0)))



(defclass s-xor (s)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))
   (sub-state-bit :accessor sub-state-bit :initarg :sub-state-bit
		  :initform 0)
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))))

(defmethod initialize-instance :after ((s s-xor) &key)
  (when (sub-state s)
    (setf (sub-state-bit s) (state-bit (sub-state s)))))

(defclass history-s-xor (s-xor) ())


(defclass s-and (s)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))
   (sub-states-bits :accessor sub-states-bits :initarg :sub-states-bits
		    :initform 0)))

(defmethod initialize-instance :after ((s s-and) &key)
  (setf (sub-states-bits s) (reduce #'+ (sub-states s) :key #'state-bit)
	(sub-states s) (sort (sub-states s) #'string< :key #'name)))


(defgeneric copy-state (s &key))

(sc.utils::define-copy-object-method (s copy-state)
  name defining-state on-entry on-reentry on-exit
  state-bit default-state-bit)

(sc.utils::define-copy-object-method (s-xor copy-state)
  name defining-state on-entry on-reentry on-exit
  sub-state sub-state-bit default-state)

(sc.utils::define-copy-object-method (s-and copy-state)
  name defining-state on-entry on-reentry on-exit
  sub-states sub-states-bits)


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
    (format stream "[~a]" (sc.utils::integer->bit-vector (state-bit obj)))
    (print-s obj stream)))

(defmethod print-s ((obj s) stream)
  (format stream "(~a)" (name obj)))

(defmethod print-s ((obj s-xor) stream)
  (if (sub-state obj)
      (progn
	(format stream "(~a " (name obj))
	(print-s (sub-state obj) stream)
	(format stream ")"))
      (format stream "(~a" (name obj))))


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



(defgeneric key-subset-of-state (s state-key))

(defmethod key-subset-of-state ((s s) state-key)
  (= (logand (sc.key::state-bits state-key)
	     (state-bit s))
     (sc.key::state-bits state-key)))

(defmethod state-subset-of-key ((s s) state-key)
  (= (logand (sc.key::state-bits state-key)
	     (state-bit s))
     (state-bit s)))

;; remove substates not explicitly speficied in state-key
(defgeneric remove-implicit-substates (state state-key)
  (:documentation "Remove substates from STATE not explicitly speficied in STATE-NAME."))

(defmethod remove-implicit-substates ((s (eql nil)) state-key)
  (error "Substate(s) given in state-key not found in state."))

(defun check-state-key (state state-key)
  (when (not (state-subset-of-key state state-key))
    (error "State: ~a not described by state-name: ~a" state state-key)))

(defmethod remove-implicit-substates ((state s) state-key)
  (copy-state state))

(defmethod remove-implicit-substates ((state s-xor) state-key)
  (cond
    ;; the name matches but doesn't specify any sub-states -> return technically, this
    ;; doesn't make any sense but for determining which state is reentered it might be
    ;; useful
    ((= (logand (sc.key::state-bits state-key)
		(sub-state-bit state))
	0)
     (copy-state state :sub-state nil))
    ;; the name still matches and specifies sub-states -> test substate
    (t (copy-state state :sub-state
		   (remove-implicit-substates (sub-state state) state-key)))))


(defmethod remove-implicit-substates ((state s-and) state-key)
  (cond
    ;; the state-key doesn't specify any substates, so return nil
    ((= (logand (sc.key::state-bits state-key)
		(sub-states-bits state))
	0)
     (copy-state state :sub-states nil))
    ;; name matches and we have substates, so return only
    ;; the ones given in state-key
    (t
     (copy-state state :sub-states
		 (iter
		   (for s in (sub-states state))
		   (when (> (logand (sc.key::state-bits state-key)
				    (state-bit s))
			    0)
		     (collect (remove-implicit-substates s state-key))))))))



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



(defgeneric is-partial-default-state (s to-resolve-on))


(defmethod is-partial-default-state ((s s) to-resolve-on)
  (= (logand (state-bit s) to-resolve-on)
     (logand (default-state-bit s) to-resolve-on)))

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



(defun get-states-described-by-name (lst-of-states state-key)
  (remove-if-not #'(lambda (s) (state-subset-of-key s state-key))
		 lst-of-states))


(defun resolve-final-state (possible-states state-key)
  (let+ (;; in theory, these are more states than necessary as not all of
	 ;; history-states will have exits, but keeping track of this is
	 ;; bothersome; fixme: if this should be a performance issue, we might
	 ;; wanna fix it
	 (history-states (remove-if-not #'is-history-state possible-states))
	 (bits-to-resolve-on (sc.key::excluded-state-bits state-key))
	 (resolved-states
	  (remove-if-not #'(lambda (s) (is-partial-default-state s bits-to-resolve-on))
			 possible-states))
	 (final-state resolved-states))
    (unless (= (length final-state) 1)
      (error "Implementation error resolving default state for: ~a" state-key))
    (values (first resolved-states) history-states)))


