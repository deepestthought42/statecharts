(in-package #:statecharts)

(declaim (optimize (debug 3) (speed 0) (space 0)))

;;; definitions for objects used during parsing / creation of fsm

(defun state-p (obj)
  (typep obj 'state))


(defclass s ()
  ((name :initarg :name :accessor name :initform (error "Must initialize name."))
   (defining-state :initarg :defining-state :accessor defining-state 
		   :initform (error "Must initialize defining-state."))
   (on-entry :accessor on-entry :initarg :on-entry :initform '())
   (on-exit :accessor on-exit :initarg :on-exit :initform '())))


(defclass s-xor (s)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))))

(defclass s-and (s)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))


(defclass tr ()
  ((initial-state-name :initarg :initial-state-name :accessor initial-state-name 
		       :initform (error "Must initialize initial-state-name."))
   (final-state-name :initarg :final-state-name :accessor final-state-name 
		     :initform (error "Must initialize final-state-name."))
   (event-name :initarg :event-name :accessor event-name 
	      :initform (error "Must initialize event-state-name."))
   (guard :initarg :guard :accessor guard 
	  :initform (error "Must initialize guard."))))


;;; state to name comparison

(defgeneric state-described-by-name (s name))

(defmethod state-described-by-name ((s t) name) nil)

(defmethod state-described-by-name ((s s) state-name)
  (string= (name s) (name state-name)))

(defmethod state-described-by-name ((s s-xor) state-name)
  (cond
    ;; name and state-name have to match as well as the type of name
    ((not (and (string= (name s) (name state-name))
	       (typep state-name 'or-state-name)))
     nil)
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
      ((not (and (string= (name s) (name state-name))
		 (typep state-name 'and-state-name)))
       nil)
      ;; the state-name doesn't specify any substates, so all good
      ((not (sub-states state-name)) t)
      ;; name matches and we have substates, so compare them one be one
      (t (reduce #'(lambda (a b) (and a b))
		 (sub-states state-name)
		 :key #'is-sub-state)))))


;;; parse statecharts

(defgeneric compute-substates (s))

(defmethod compute-substates ((s t)) '())

(defmethod compute-substates ((s state))
  (list (make-instance 's :name (name s) :defining-state s)))

(defmethod compute-substates ((cluster cluster))
  (let+ (((&slots name elements transitions default-state) cluster))
    (iter outer
      (for e in elements)
      (for sub-states = (compute-substates e))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance 's-xor :name name :sub-state s
				    :defining-state cluster
				    :default-state default-state)))))))

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
						    :defining-state ortho
						    :name name))
			  (car super-lst)))
		 (t
		  (let ((states-1 (car super-lst))
			(states-n (combine-elements (rest super-lst))))
		    (iter
		      (for s-1 in states-1)
		      (appending
		       (mapcar #'(lambda (s-n)
				   (make-instance 's-and :name name
							 :defining-state ortho
							 :sub-states
							 (append (list s-1)
								 (sub-states s-n))))
			       states-n))))))))
      (combine-elements lst-of-lst-of-substates))))


;;; transitions






(defmethod compute-transitions ((s t) super-state) '())

(defun %make-transitions (elements super-state)
  (iter
    (for el in elements)
    (when (typep el 'transition)
      (collect
	  (make-instance 'tr
			 :event-name (event el)
			 :guard (guard el)
			 :initial-state-name
			 (%dereference-key super-state
					   (initial-state el))
			 :final-state-name
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
	    (initial-state-name obj)
	    (event-name obj)
	    (final-state-name obj))))


(defmethod print-object ((obj s) stream)
  (print-unreadable-object (obj stream)
    (print-s obj stream)))

(defmethod print-s ((obj s) stream)
  (format stream "~a" (name obj)))

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



;;; these methods assume states S that correspond to the name given

(defgeneric %walk-state (s name)
  (:method ((s t) name) nil))

(defmethod %walk-state ((s s) name) t)

(defmethod %walk-state ((s s-xor) name)
  (if (cdr name)
      (%walk-state (sub-state s) (cadr name))
      (is-default-state s)))


(defmethod %walk-state ((s s-and) name)
  (labels ((find-key (sub-s key-list)
	     (iter
	       (for k in key-list)
	       (if (state-described-by-name sub-s k)
		   (return k)))))
    (let+ (((&slots sub-states) s)
	   (keys (cond
		   ((and (listp (cadr name))
			 (equal :and (caadr name)))
		    (cdadr name))
		   ((and (listp (cadr name))
			 (stringp (caadr name)))
		    (cdr name))
		   (t (error 'invalid-state-descriptor :name name)))))
      ;; given the name definition it can be of one of the following
      ;; forms:
      ;; ("X" (:and ("Z" ..) ..))
      ;; ("X" ("Z" ..))
      (iter
	(for sub-s in sub-states)
	(for k = (find-key sub-s keys))
	(cond
	  ((and k (not (%walk-state sub-s k))) (return nil))
	  ((and (not k) (not (is-default-state sub-s))) (return nil)))
	(finally (return t))))))

(defun get-partial-default-state (lst-of-states state-key)
  (let+ ((described-states
	  (remove-if-not #'(lambda (s)
			     (state-described-by-name s state-key))
			 lst-of-states)))
    (remove-if-not #'(lambda (s) (%walk-state s state-key)) described-states)))

;;; graph of states

(defclass node ()
  ((name :initarg :name :accessor name 
	 :initform (error "Must initialize name."))
   (default :initarg :default :accessor default 
	    :initform (error "Must initialize default."))
   (nodes :accessor nodes :initarg :nodes :initform '())))


(defgeneric compute-graph (el name list-of-states))


;;; accumulate events

(defun gather-events-from-transitions (transitions)
  (let+ ((table (make-hash-table :test 'equal)))
    (map nil #'(lambda (tr)
		 (if (gethash (event-name tr) table)
		     (push tr (gethash (event-name tr) table))
		     (setf (gethash (event-name tr) table) (list tr))))
	 transitions)
    table))


(defun set-transition (initial-state event-name final-state))



(defun described-by-final-keys? (states keys)
  (iter
    (for k in keys)
    (for ss initially states
	 then (remove-if-not #'(lambda (s)
				 (state-described-by-name s k))
			     ss))
    (until (not ss))
    (finally (return ss))))



(defun get-final-state (states transitions)
  (let+ ((len (length transitions))
	 ((&values name final-state)
	  (case len
	    (0 (error "This shouldn't happen."))
	    (1 (let ((name (final-state-name (car transitions))))
		 (values name (get-partial-default-state states name))))
	    (t (progn
		 (break)
		 (error "not implemented."))))))
    (if final-state
	final-state
	(error 'invalid-state-descriptor :descriptor name))))



(defun find-final-states-for-transitions (states transitions)
  (labels ((find-events/transition-originating-from-state (s)
	     (group-by:group-by (remove-if-not
				 #'(lambda (tr)
				     (state-described-by-name s (initial-state-name tr)))
				 transitions)
				:key #'event-name :value #'identity)))
    (iter outer
      (for s in states)
      ;; for every state s
      (iter
	;; for every event-name and transitions originating in s
	(for ev.transitions
	     in (find-events/transition-originating-from-state s))
	(for ev = (car ev.transitions))
	(for trans = (cdr ev.transitions))
	(when trans
	  (in outer
	      (set-transition s ev (get-final-state states trans))))))))



