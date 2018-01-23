(in-package #:statecharts)


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



