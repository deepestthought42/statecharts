(in-package #:statecharts)
#+nil
(declaim (optimize (debug 3) (speed 0) (space 0)))



(defun state-p (obj)
  (typep obj 'state))


(defclass s ()
  ((name :initarg :name :accessor name :initform (error "Must initialize name."))
   (defining-state :initarg :defining-state :accessor defining-state 
		   :initform nil)
   (on-entry :accessor on-entry :initarg :on-entry :initform '())
   (on-reentry :accessor on-reentry :initarg :on-reentry :initform '())
   (on-exit :accessor on-exit :initarg :on-exit :initform '())))

(defclass s-xor (s)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))))

(defclass s-and (s)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))


(defgeneric copy-state (s &key)
  (:method ((s s) &key (name (name s)) (defining-state (defining-state s))
		       (on-entry (on-entry s)) (on-reentry (on-reentry s))
		       (on-exit (on-exit s)))
    (make-instance 's :name name
		      :defining-state defining-state
		      :on-entry on-entry
		      :on-reentry on-reentry
		      :on-exit on-exit))
  (:method ((s s-xor) &key (name (name s)) (defining-state (defining-state s))
			   (on-entry (on-entry s)) (on-reentry (on-reentry s))
			   (on-exit (on-exit s)) (sub-state (sub-state s))
			   (default-state (default-state s)))
    (make-instance 's-xor :name name
			  :defining-state defining-state
			  :on-entry on-entry
			  :on-reentry on-reentry
			  :on-exit on-exit
			  :sub-state sub-state
			  :default-state default-state))
  (:method ((s s-and) &key (name (name s)) (defining-state (defining-state s))
			   (on-entry (on-entry s)) (on-reentry (on-reentry s))
			   (on-exit (on-exit s)) (sub-states (sub-states s)))
    (make-instance 's-and :name name
			  :defining-state defining-state
			  :on-entry on-entry
			  :on-reentry on-reentry
			  :on-exit on-exit
			  :sub-states sub-states)))



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





(defclass tr ()
  ((initial-state-name :initarg :initial-state-name :accessor initial-state-name 
		       :initform (error "Must initialize initial-state-name."))
   (final-state-name :initarg :final-state-name :accessor final-state-name 
		     :initform (error "Must initialize final-state-name."))
   (is-reentry :accessor is-reentry :initarg :is-reentry
	       :initform (error "Must initialize is-reentry"))
   (event-name :initarg :event-name :accessor event-name 
	       :initform (error "Must initialize event-state-name."))
   (guard :initarg :guard :accessor guard 
	  :initform (error "Must initialize guard."))))


(defclass tr-target ()
  ((on-entry-actions :initarg :on-entry-actions :accessor on-entry-actions 
		     :initform (error "Must initialize on-entry-actions."))
   (on-reentry-actions :initarg :on-reentry-actions :accessor on-reentry-actions 
		       :initform (error "Must initialize on-reentry-actions."))
   (on-exit-actions :initarg :on-exit-actions :accessor on-exit-actions 
		    :initform (error "Must initialize on-exit-actions."))
   (initial-name :initarg :initial-name :accessor initial-name 
		 :initform (error "Must initialize initial-name."))
   (final-name :initarg :final-name :accessor final-name 
	       :initform (error "Must initialize final-name."))
   (fsm-state :initarg :fsm-state :accessor fsm-state 
	      :initform (error "Must initialize fsm-state."))))


;;; printing

(defmethod print-object ((obj tr) stream)
  (print-unreadable-object (obj stream)
    (print-state-name (initial-state-name obj) stream)
    (format stream " --|~a|--> " (event-name obj))
    (print-state-name (final-state-name obj) stream)))


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

(defmethod  state=state-name ((state s)
			      (state-name state-name))
  (string= (name state-name)
	   (name state)))

(defmethod  state=state-name ((state s-xor)
			      (state-name or-state-name))
  (string= (name state-name)
	   (name state)))

(defmethod  state=state-name ((state s-and)
			      (state-name and-state-name))
  (string= (name state-name)
	   (name state)))



(defgeneric state-described-by-name (s state-name))

(defmethod state-described-by-name ((s t) state-name) nil)

(defmethod state-described-by-name ((s s) state-name)
  (state=state-name s state-name))

(defmethod state-described-by-name ((s s-xor) state-name)
  (cond
    ;; name and state-name have to match as well as the type of name
    ((not (state=state-name s state-name)) nil)
    ;; the name matches but doesn't specify any sub-states -> match
    ((not (sub-state state-name)) t)
    ;; the name still matches and specifies sub-states -> test substate
    (t (state-described-by-name (sub-state s) (sub-state state-name)))))

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
      ((not (sub-states state-name)) t)
      ;; name matches and we have substates, so compare them one be one
      (t (reduce #'(lambda (a b) (and a b))
		 (sub-states state-name)
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
    ((not (sub-state state-name)) (copy-state state :sub-state nil))
    ;; the name still matches and specifies sub-states -> test substate
    (t (copy-state state :sub-state
		   (remove-implicit-substates (sub-state state) (sub-state state-name))))))


(defmethod remove-implicit-substates ((state s-and) state-name)
  (check-state-name state state-name)
  (cond
    ;; the state-name doesn't specify any substates, so return nil
    ((not (sub-states state-name)) (copy-state state :sub-states nil))
    ;; name matches and we have substates, so return only
    ;; the ones given in state-name
    (t
     (copy-state state :sub-states
		 (iter
		   (for sn in (sub-states state-name))
		   (for s = (find (name sn) (sub-states state) :test #'string= :key #'name))
		   (when (not s) (error "Couldn't find substate with name: ~a" sn))
		   (for explicit = (remove-implicit-substates s sn))
		   (when explicit (collect explicit)))))))

;;; create state-name from s

(defgeneric create-state-name (s))

(defmethod create-state-name ((s s))
  (make-instance 'state-name :name (name s)))

(defmethod create-state-name ((s s-xor))
  (make-instance 'or-state-name
		 :name (name s)
		 :sub-state (create-state-name (sub-state s))))

(defmethod create-state-name ((s s-and))
  (make-instance 'and-state-name
		 :name (name s)
		 :sub-states (mapcar #'create-state-name (sub-states s))))

;;; default states

(defmethod is-default-state ((s t)) nil)


(defmethod is-default-state ((s s)) t)


(defmethod is-default-state ((s s-xor))
  (let+ (((&slots default-state sub-state) s))
    (cond
      ((string= (name sub-state) default-state)
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
  (if (= (id (defining-state a))
	 (id (defining-state b)))
      (values '() '())
      (values (list a) (list b))))

(defmethod difference ((a s-xor) (b s-xor))
  (if (= (id (defining-state a))
	 (id (defining-state b)))
      (difference (sub-state a) (sub-state b))
      (values (list a) (list b))))


(defmethod difference ((a s-and) (b s-and))
  (labels ((d-id (s) (id (defining-state s))))
    (cond
      ((= (d-id a) (d-id b))
       (if (not (= (length (sub-states a))
		   (length (sub-states b))))
	   (error "This shouldn't be possible."))
       (iter
	 (for sub-a in (sort (sub-states a) #'< :key #'d-id))
	 (for sub-b in (sort (sub-states b) #'< :key #'d-id))
	 (for (values in-a in-b) = (difference sub-a sub-b))
	 (appending in-a into in-a-but-not-b)
	 (appending in-b into in-b-but-not-a)
	 (finally
	  (return
	    (values in-a-but-not-b in-b-but-not-a)))))
      (t (list a) (list b)))))



;;; these methods assume states S that correspond to the name given

(defgeneric %is-partial-default-state (s state-name))

(defmethod %is-partial-default-state ((s s) (state-name state-name)) t)

(defmethod %is-partial-default-state ((s s-xor) (state-name or-state-name))
  (if (sub-state state-name)
      (%is-partial-default-state (sub-state s) (sub-state state-name))
      (is-default-state s)))


(defmethod %is-partial-default-state ((s s-and) (state-name and-state-name))
  (labels ((find-state-name (sub-s state-names)
	     (iter
	       (for state-name in state-names)
	       (if (state-described-by-name sub-s state-name)
		   (return state-name)))))
    (iter
      (for sub-s in (sub-states s))
      (for s-name = (find-state-name sub-s (sub-states state-name)))
      (cond
	((and s-name (not (%is-partial-default-state sub-s s-name))) (return nil))
	((and (not s-name) (not (is-default-state sub-s))) (return nil)))
      (finally (return t)))))


(defun get-states-described-by-name (lst-of-states state-name)
  (remove-if-not #'(lambda (s) (state-described-by-name s state-name))
		 lst-of-states))

(defun get-partial-default-state (lst-of-states state-name)
  (let+ ((described-states (get-states-described-by-name lst-of-states state-name)))
    (first (remove-if-not #'(lambda (s) (%is-partial-default-state s state-name)) described-states))))


