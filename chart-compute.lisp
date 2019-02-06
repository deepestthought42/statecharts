(in-package #:statecharts)


;;; parse statecharts

(defgeneric compute-substates (s))

(defmethod compute-substates ((s t)) '())

(defmethod compute-substates ((s state))
  (list (make-instance 's :name (name s)
			  :defining-state s
			  :on-entry (on-entry s)
			  :on-reentry (on-reentry s)
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
					   :on-reentry (on-reentry cluster)
					   :on-exit (on-exit cluster)
					   :default-state default-state)))))))

(defmethod compute-substates ((ortho orthogonal))
  (let+ (((&slots name elements) ortho)
	 (lst-of-lst-of-substates
	  (remove-if #'not
		     (mapcar #'(lambda (s) (compute-substates s)) elements))))
    (labels ((combine-elements (super-lst) 
	       (cond
		 ((not (cdr super-lst))
		  (mapcar #'(lambda (s)
			      (make-instance 's-and :sub-states (list s)
						    :defining-state ortho
						    :on-entry (on-entry ortho)
						    :on-reentry (on-reentry ortho)
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
							 :on-reentry (on-reentry ortho)
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
      (for tr = (make-instance 'tr
			       :event-name (event-symbol el)
			       :guard (guards el)
			       :initial-state-name
			       (make-state-name (initial-state el) chart-element super-state)
			       :final-state-name
			       (make-state-name (final-state el) chart-element super-state)))
      (collect tr))))


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
	    (alexandria:mappend #'(lambda (s) (recursive-accumulation s accessor)) (sub-states s)))))



(defun determine-entry/exit-actions (initial final)
  "Given initial and final state of a transition (in INITIAL and FINAL), determines the
  difference in states (i.e. the states exited and entered) between the initial and final
  state. Collects all /on-exit/ actions for all sub-states of INITIAL that are not in FINAL
  and all /on-entry/ actions for the sub-states that are not in INITIAL but in FINAL:

  + returns :: on-exit*, on-entry*"
  (labels ((acc (states accessor)
	     (alexandria:mappend #'(lambda (s) (remove-if #'not (recursive-accumulation s accessor)))
				 states)))
    (let+ (((&values in-initial-but-not-final
		     in-final-but-not-initial)
	    (difference initial final))
	   (is-reentry (eql initial final)))
      (values (acc in-initial-but-not-final #'on-exit)
	      (acc in-final-but-not-initial #'on-entry)
	      (when is-reentry (acc (list initial) #'on-reentry))))))





(defun set-transition (fsm-state initial-state event-name
		       final-state final-fsm-state)
  (let+ (((&values on-exit-actions on-entry-actions on-reentry-actions)
	  (determine-entry/exit-actions initial-state final-state)))
    (pushnew (cons event-name
		   (make-instance 'tr-target
				  :on-entry-actions on-entry-actions
				  :on-exit-actions on-exit-actions
				  :on-reentry-actions on-reentry-actions
				  :initial-name (create-state-name initial-state)
				  :final-name (create-state-name final-state)
				  :fsm-state final-fsm-state))
	     (ev->state fsm-state)
	     :test #'equal
	     :key #'first)))


(defun determine-final-states (all-states initial-state transitions)
  ;;; probably not the most effective, but it seems to work. there is also a lot of
  ;;; consing going on here, so if we find that we have a performance problem upon
  ;;; creating a sc, start here.
  (let+ (((&values trans-init-state-name trans-final-state-name)
	  ;; join the transitions names if more than one transition exists; if we have
	  ;; transitions that are incompatible, it should be noticed when joining the
	  ;; names
	  (cond ((not transitions)
		 (error "No transitions given found for initial state: ~a ?" initial-state))
		((= (length transitions) 1)
		 (values (initial-state-name (first transitions))
			 (final-state-name (first transitions))))
		(t
		 (values (reduce #'join-state-names (mapcar #'initial-state-name transitions))
			 (reduce #'join-state-names (mapcar #'final-state-name transitions))))))
	 ;; try finding states that are described by TRANS-FINAL-STATE-NAME
	 (possible-final-state-names
	  (alexandria:if-let (tfsn (get-states-described-by-name all-states trans-final-state-name))
	    tfsn (error "Couldn't find states described by final-state: ~a of transition: ~a"
			trans-final-state-name trans-final-state-name)))
	 ;; since we work on state-names, create one for INITIAL-STATE
	 (full-initial-state-name (create-state-name initial-state))
	 ;; calculate the states that are defined in the current state (as given by
	 ;; FULL-INITIAL-STATE-NAME) but are not described by the initial-state of
	 ;; TRANSITIONS
	 (in-current-state-but-not-trans (difference-state-names full-initial-state-name
								 trans-init-state-name))
	 ;; select FINAL-STATES from possible states such that orthogonal states not
	 ;; affected by TRANSITIONS stay the same 
	 (final-states
	  (cond
	    ;; FULL-INITIAL-STATE-NAME might be a more specific or state, so it will be
	    ;; return, e.g: ("A" ("Z" "X")), ("A" "Y") => ("A" ("Z" "X"))
	    ;;
	    ;; if that is the case, all states described by the final-state-name from
	    ;; TRANSITIONS are possible
	    ((and in-current-state-but-not-trans (state-name= in-current-state-but-not-trans
							      full-initial-state-name))
	     possible-final-state-names)
	    ;; when we have orthogonal states not described by TRANSITIONS, e.g.:
	    ;;
	    ;; ("A" ("A" "A")^("B" "A")) with transition defined as ("A" ("A" "A")) ->
	    ;; ("A" ("A" "B"))
	    
	    (in-current-state-but-not-trans (get-states-described-by-name possible-final-state-names
									  in-current-state-but-not-trans))
	    ;; no orthogonal states involved
	    (t possible-final-state-names))))
    (cond
      ((not final-states) (error "Couldn't find final state ?"))
      ;; final states not completely specified, need to select default state
      ((> (length final-states) 1)
       (let ((final-from-partial (get-partial-default-state final-states trans-final-state-name)))
	 (unless final-from-partial
	   (error "Couldn't determine default states for final states."))
	 final-from-partial))
      ;; final state completely specified
      (t (first final-states)))))








(defun set-transitions-for-fsm-states (all-states all-fsm-states all-transitions)
  (labels ((find-events/transition-originating-from-state (state)
	     (group-by:group-by (remove-if-not
				 #'(lambda (tr)
				     (state-described-by-name state (initial-state-name tr)))
				 all-transitions)
				:key #'event-name :value #'identity))
	   (find-fsm-state-for (state)
	     (alexandria:if-let (ret (find (create-state-name state) all-fsm-states
					   :key #'name :test #'state-name=))
	       ret (error "Huh ? couldn't find fsm state for state: ~a" state))))
    (iter 
      (for initial-state in all-states)
      (iter
	;; for every EVENT-NAME and TRANSITIONS* originating in INITIAL-STATE
	(for (event-name . transitions) in (find-events/transition-originating-from-state initial-state))
	(when transitions
	  ;; find FINAL-STATE when applying TRANSITIONS* to INITIAL-STATE
	  (for final-state = (determine-final-states all-states initial-state transitions))
	  (set-transition (find-fsm-state-for initial-state)
			  initial-state event-name
			  final-state
			  (find-fsm-state-for final-state)))))))





