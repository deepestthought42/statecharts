(in-package #:statecharts)

(declaim (optimize (debug 3) (speed 0) (space 0)))

;;; definitions for objects used during parsing / creation of fsm

(defun state-p (obj)
  (typep obj 'state))


(defclass s ()
  ((key :initarg :key :accessor key 
	:initform (error "Must initialize key."))))


(defclass s-xor (s)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))))

(defclass s-and (s)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))


(defclass tr ()
  ((initial-key :initarg :initial-key :accessor initial-key 
		:initform (error "Must initialize initial-key."))
   (final-key :initarg :final-key :accessor final-key 
	      :initform (error "Must initialize final-key."))
   (event-key :initarg :event-key :accessor event-key 
	      :initform (error "Must initialize event-key."))
   (guard :initarg :guard :accessor guard 
	  :initform (error "Must initialize guard."))))


;;; state to key comparison

(defgeneric key-describes-state (s key))

(defmethod key-describes-state ((s t) key) nil)

(defun %compare-key (state key)
  (equal (key state)
	 (cond
	   ((listp key) (car key))
	   (t key))))

(defmethod key-describes-state ((s s) key)
  (%compare-key s key))

(defmethod key-describes-state ((s s-xor) key)
  (and (%compare-key s (car key))
       (if (not (cadr key)) ;; key finished ?
	   t
	   (key-describes-state (sub-state s)
				(cadr key)))))

(defmethod key-describes-state ((s s-and) key)
  (labels ((is-sub-state (k)
	     (iter
	       (for sub-s in (sub-states s))
	       (if (key-describes-state sub-s k)
		   (return t)))))
    (and (%compare-key s (cond
			   ((listp key) (car key))
			   ((stringp key) key)
			   (t (error 'invalid-state-descriptor :descriptor key))))
	 (cond
	   ;; key finished ?
	   ((or (not key)
		;; if a string is given, the comparison has been done
		;; in %compare-key
		(stringp key))
	    t)
	   ;; need :and next
	   ((equal :and (caadr key))
	    (reduce #'(lambda (a b)
			  (and a b))
		      (cdadr key)
		      :key #'is-sub-state))
	   (t (error "?"))))))


;;; parse statecharts

(defgeneric compute-substates (s))

(defmethod compute-substates ((s t)) '())

(defmethod compute-substates ((s state))
  (list (make-instance 's
		       :key (name s))))

(defmethod compute-substates ((cluster cluster))
  (let+ (((&slots name elements transitions default-state) cluster))
    (iter outer
      (for e in elements)
      (for sub-states = (compute-substates e))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance 's-xor :key name :sub-state s
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
  (labels ((syntactically-valid-descriptor (desc)
	     (cond
	       ((stringp desc) t)
	       ((symbolp desc) t)
	       ((and (listp desc)
		     (equal :and (first desc)))
		(mapcar #'syntactically-valid-descriptor (cdr desc)))
	       ((and (listp desc)
		     (stringp (first desc)))
		(mapcar #'syntactically-valid-descriptor desc))
	       (t (error 'invalid-state-descriptor :descriptor desc)))))
    (cond
      ;; (/ "A" "B" "C") references with respect to the root of the tree:
      ;; -> "C" within "B" within "A"
      ((and (listp key) (equal '/ (first key))
	    (syntactically-valid-descriptor (cdr key)))
       (cdr key))
      ;; ("A" "B") references within the current super state:
      ;; "B" within "A" within the current superstate
      ((and (listp key)
	    (syntactically-valid-descriptor key)
	    (or (equal nil superstate)
		(syntactically-valid-descriptor superstate)))
       (append superstate key))
      ;; "A" references "A" within the current superstate
      ((and (not (listp key))
	    (syntactically-valid-descriptor key))
       (append superstate (list key)))
      (t (error 'invalid-state-descriptor
		:descriptor key)))))


(defmethod compute-transitions ((s t) super-state) '())

(defun %make-transitions (elements super-state)
  (iter
    (for el in elements)
    (when (typep el 'transition)
      (collect
	  (make-instance 'tr
			 :event-key (event el)
			 :guard (guard el)
			 :initial-key
			 (%dereference-key super-state
					   (initial-state el))
			 :final-key
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
	    (initial-key obj)
	    (event-key obj)
	    (final-key obj))))


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





;;; default states

(defmethod is-default-state ((s t) &key of-state-key)
  (declare (ignore of-state-key))
  nil)


(defmethod is-default-state ((s s) &key of-state-key)
  (cond
    ;; it is a leaf state, so we just check if the key, if given, is
    ;; correct
    (of-state-key (key-describes-state s of-state-key))
    (t t)))


(defmethod is-default-state ((s s-xor) &key of-state-key)
  (let+ (((&slots default-state sub-state) s))
    (cond
      ;; so, we have a state-key to compare to 
      (of-state-key
       (and
	;; make sure this is a state corresponding to of-state-key
	(key-describes-state s of-state-key)
	;; make sure it is the default state
	(string= (key sub-state) default-state)
	;; make sure it's sub-states are defaults states too
	(is-default-state sub-state
			  :of-state-key
			  (cadr of-state-key))))
      ;; no state key, ==> t if sub state is default state
      ((string= (key sub-state) default-state)
       (is-default-state sub-state))
      ;; no default state
      (t nil))))


(defmethod is-default-state ((s s-and) &key of-state-key)
  (labels ((substates-are-default ()
	     (reduce #'(lambda (a b)
			 (and a b))
		     (sub-states s)
		     :key #'(lambda (s)
			      (is-default-state
			       s :of-state-key
			       (cond
				 ;; key can be leaf 
				 ((listp of-state-key) (cadr of-state-key))
				 ((stringp of-state-key) '())
				 (t (error 'invalid-state-descriptor
					   :key of-state-key))))))))
    (cond
      ;; so, we have a state-key to compare to 
      (of-state-key
       (and
	;; make sure this is a state corresponding to of-state-key
	(key-describes-state s of-state-key)
	;; make sure it's sub-states are defaults states too
	(substates-are-default)))
      ;; no state key, ==> t if S is default state
      (t (substates-are-default)))))






