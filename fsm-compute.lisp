(in-package #:statecharts)


;;; parse statecharts

(defgeneric compute-substates (s))

(defmethod compute-substates ((s t)) '())

(defmethod compute-substates ((s state))
  (list (make-instance 's :name (name s)
			  :defining-state s
			  :on-entry (on-entry s)
			  :on-exit (on-exit s))))

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
					   :on-entry (on-entry cluster)
					   :on-exit (on-exit cluster)
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
						    :on-entry (on-entry ortho)
						    :on-exit (on-exit ortho)
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
							 :on-entry (on-entry ortho)
							 :on-exit (on-exit ortho)
							 :sub-states
							 (append (list s-1)
								 (sub-states s-n))))
			       states-n))))))))
      (combine-elements lst-of-lst-of-substates))))


;;; transitions

(defmethod compute-transitions ((s t) super-state chart-element) '())

(defun %make-transitions (elements super-state chart-element)
  (iter
    (for el in elements)
    (when (typep el 'transition)
      (collect
	  (make-instance 'tr
			 :event-name (event-symbol el)
			 :guard (guards el)
			 :initial-state-name
			 (make-state-name (initial-state el) chart-element super-state)
			 :final-state-name
			 (make-state-name (final-state el) chart-element super-state))))))


(defmethod compute-transitions ((s cluster) super-state chart-element)
  (let+ (((&slots name elements) s)
	 (super-state (append super-state (list name)))
	 (transitions-for-sub-states
	  (iter
	    (for el in elements)
	    (appending (compute-transitions el super-state chart-element)))))
    (append transitions-for-sub-states
	    (%make-transitions elements super-state chart-element))))

(defmethod compute-transitions ((s orthogonal) super-state chart-element)
  (let+ (((&slots name elements) s)
	 (super-state (append super-state (list name)))
	 (transitions-for-sub-states
	  (iter
	    (for el in elements)
	    (appending (compute-transitions el super-state chart-element)))))
    (append transitions-for-sub-states
	    (%make-transitions elements super-state chart-element))))


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



(defun set-transition (initial-state event-name final-state)
  (pushnew (cons event-name
		 (make-instance 'tr-target
				:state final-state
				:actions (append (on-exit initial-state)
						 (on-entry final-state))
				:initial-name (create-state-name initial-state)
				:final-name (create-state-name final-state)))
	   (ev->state initial-state)
	   :test #'equal))



(defun described-by-final-keys? (states state-names)
  (iter
    (for sn in state-names)
    (for ss initially states
	 then (remove-if-not #'(lambda (s)
				 (state-described-by-name s sn))
			     ss))
    (until (not ss))
    (finally (return ss))))





(defun get-final-state (states transitions)
  (let+ ((len (length transitions))
	 ((&values name final-state)
	  (case len
	    (0 (error "This shouldn't happen."))
	    (1 (let ((name (final-state-name (first transitions))))
		 (values name (get-partial-default-state states name))))
	    (t (let ((name (reduce #'join-state-names
				   (mapcar #'final-state-name transitions))))
		 (values name (get-partial-default-state states name)))))))
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
	  (progn ;;handler-case
	      (in outer
		  (set-transition s ev (get-final-state states trans)))
	    ;; (t (c)
	    ;;   (error 'couldnt-determine-final-state
	    ;; 	     :initial-state s
	    ;; 	     :event ev
	    ;; 	     :given-condition c))
	    ))))))




(defgeneric flatten-state (s new-state))

(defun %flatten-state (s new-state)
  (macrolet ((updatef (accessor)
	       `(let ((val (,accessor s)))
		  (cond
		    ((and val (listp val))
		     (setf (,accessor new-state)
			   (append (,accessor new-state) val)))
		    ((and val)
		     (setf (,accessor new-state)
			   (append (,accessor new-state) (list val))))))))
    (updatef on-entry)
    (updatef on-exit)
    
    (updatef defining-state)))

(defmethod flatten-state ((s s) (new-state s))
  (%flatten-state s new-state)
  new-state)

(defmethod flatten-state ((s s-and) (new-state s))
  (%flatten-state s new-state)
  (iter
    (for sub-s in (sub-states s))
    (flatten-state sub-s new-state))
  new-state)

(defmethod flatten-state ((s s-xor) (new-state s))
  (%flatten-state s new-state)
  (if (sub-state s)
      (flatten-state (sub-state s) new-state))
  new-state)


(defun flatten-all-states (states)
  (iter
    (for s in states)
    (for new-state =
	 (make-instance 's :name (create-state-name s)
			   :ev->state (copy-seq (ev->state s))))
    (flatten-state s new-state)
    (collect (cons s new-state))))

(defun replace-final-states-in-transitions (states.flattened-states)
  (labels ((flatten-transitions (u)
	     (iter
	       (for (ev . tr-target) in (ev->state u))
	       (for (state-1 . unchained) = (assoc (state tr-target) states.flattened-states))
	       (if (not unchained)
		   (error "Couldn't find replacement ? That doesn't make sense."))
	       (setf (state tr-target) unchained))
	     u))
    (iter
      (for s.u in states.flattened-states)
      (for u = (cdr s.u))
      (collect (flatten-transitions u)))))

(defun find-flattened-default-state (default-state states.flattened-states)
  (alexandria:if-let (fds (assoc default-state states.flattened-states))
    (cdr fds)
    (error "This shouldn't happen: Couldn't find flattened default state.")))








