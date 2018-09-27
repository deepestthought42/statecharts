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


;;; accumulate events

(defun gather-events-from-transitions (transitions)
  (let+ ((table (make-hash-table :test 'equal)))
    (map nil #'(lambda (tr)
		 (if (gethash (event-name tr) table)
		     (push tr (gethash (event-name tr) table))
		     (setf (gethash (event-name tr) table) (list tr))))
	 transitions)
    table))


(defun enclose-in-list-if-nec (ob)
  (if (listp ob) ob (list ob)))

(defgeneric recursive-accumulation (s accessor)
  (:method ((s s) accessor)
    (enclose-in-list-if-nec (funcall accessor s)))
  (:method ((s s-xor) accessor)
    (append (enclose-in-list-if-nec (funcall accessor s))
	    (recursive-accumulation (sub-state s) accessor)))
  (:method ((s s-and) accessor)
    (append (enclose-in-list-if-nec (funcall accessor s))
	    (mapcar #'(lambda (s) (recursive-accumulation s accessor)) (sub-states s)))))



(defun determine-entry/exit-actions (initial final)
  "Given initial and final state of a transition (in INITIAL and FINAL), determines the
  difference in states (i.e. the states exited and entered) between the initial and final
  state. Collects all /on-exit/ actions for all sub-states of INITIAL that are not in FINAL
  and all /on-entry/ actions for the sub-states that are not in INITIAL but in FINAL:

  + returns :: on-exit*, on-entry*"
  (labels ((acc (states)
	     (alexandria:mappend
	      #'(lambda (s)
		  (remove-if #'not (recursive-accumulation s #'on-exit)))
	      states)))
    (let+ (((&values in-initial-but-not-final
		     in-final-but-not-initial)
	    (difference initial final)))
      (values (acc in-initial-but-not-final) (acc in-final-but-not-initial)))))





(defun set-transition (fsm-state initial-state event-name
		       final-state final-fsm-state)
  (let+ (((&values on-exit-actions on-entry-actions)
	  (determine-entry/exit-actions initial-state final-state)))
    (pushnew (cons event-name
		   (make-instance 'tr-target
				  :on-entry-actions on-entry-actions
				  :on-exit-actions on-exit-actions
				  :initial-name (create-state-name initial-state)
				  :final-name (create-state-name final-state)
				  :fsm-state final-fsm-state))
	     (ev->state fsm-state)
	     :test #'equal
	     :key #'first)))



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



(defun set-transitions-for-fsm-states (states fsm-states transitions)
  (labels ((find-events/transition-originating-from-state (s)
	     (group-by:group-by (remove-if-not
				 #'(lambda (tr)
				     (state-described-by-name s (initial-state-name tr)))
				 transitions)
				:key #'event-name :value #'identity))
	   (find-fsm-state (s)
	     (find (create-state-name s) fsm-states
		   :key #'name :test #'state-name=)))
    (iter outer
      (for s in states)
      (for fsm-state = (find-fsm-state s))
      (if (not fsm-state)
	  (error "Huh ? couldn't find fsm state for state: ~a" s))
      ;; for every state s
      (iter
	;; for every event-name and transitions originating in s
	(for ev.transitions
	     in (find-events/transition-originating-from-state s))
	(for ev = (car ev.transitions))
	(for trans = (cdr ev.transitions))
	(for final-state = (get-final-state states trans))
	(when trans
	  (in outer
	      (set-transition fsm-state s ev final-state
			      (find-fsm-state final-state))))))))
