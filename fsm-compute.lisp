(in-package #:sc.fsm)


(defun create-states (states)
  (iter
    (for s in states)
    (collect (make-instance 'sc.fsm::state :name (sc.key::from-chart-state s)))))

(defun find-state-for (state all-fsm/states)
  (alexandria:if-let (ret (find (sc.key::from-chart-state state) all-fsm/states
				:key #'name :test #'sc.key::state=))
    ret (error "Huh ? couldn't find fsm state for state: ~a" state)))



(defun enclose-in-list-if-necessary (ob)
  (if (listp ob) ob (list ob)))

(defgeneric recursive-accumulation (s accessor)
  (:method ((s sc.chart::s) accessor)
    (enclose-in-list-if-necessary (funcall accessor s)))
  (:method ((s sc.chart::s-xor) accessor)
    (append (enclose-in-list-if-necessary (funcall accessor s))
	    (recursive-accumulation (sc.chart::sub-state s) accessor)))
  (:method ((s sc.chart::s-and) accessor)
    (append (enclose-in-list-if-necessary (funcall accessor s))
	    (alexandria:mappend #'(lambda (s) (recursive-accumulation s accessor)) (sc.chart::sub-states s)))))



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
	    (sc.key::difference initial final)))
      (values (acc in-initial-but-not-final #'sc.dsl::on-exit)
	      (acc in-final-but-not-initial #'sc.dsl::on-entry)))))





(defun determine-final-states (all-states initial-state transitions)
  ;;; probably not the most effective, but it seems to work. there is also a lot of
  ;;; consing going on here, so if we find that we have a performance problem upon
  ;;; creating a sc, start here.
  (let+ (((&values trans-init-state-name trans-final-state-name)
	  ;; join the transitions names if more than one transition exists; if we have
	  ;; transitions that are incompatible, it should be noticed when joining the
	  ;; names
	  (cond ((not transitions)
		 (error "No transitions found for initial state: ~a ?" initial-state))
		((= (length transitions) 1)
		 (values (sc.fsm::initial-state-name (first transitions))
			 (sc.fsm::final-state-name (first transitions))))
		(t
		 (values (reduce #'sc.key::join (mapcar #'sc.fsm::initial-state-name transitions))
			 (reduce #'sc.key::join (mapcar #'sc.fsm::final-state-name transitions))))))
	 ;; try finding states that are described by TRANS-FINAL-STATE-NAME
	 (possible-final-state-names
 	  (alexandria:if-let (tfsn (sc.chart::get-states-described-by-name
				    all-states trans-final-state-name))
	    tfsn (error "Couldn't find states described by final-state: ~a of transition: ~a"
			trans-final-state-name trans-final-state-name)))
	 ;; since we work on state-names, create one for INITIAL-STATE
	 (full-initial-state-name (sc.key::from-chart-state initial-state))
	 ;; calculate the states that are defined in the current state (as given by
	 ;; FULL-INITIAL-STATE-NAME) but are not described by the initial-state of
	 ;; TRANSITIONS
	 (in-current-state-but-not-trans (sc.key::difference full-initial-state-name
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
	    ((and in-current-state-but-not-trans (sc.key::state= in-current-state-but-not-trans
							      full-initial-state-name))
	     possible-final-state-names)
	    ;; when we have orthogonal states not described by TRANSITIONS, e.g.:
	    ;;
	    ;; ("A" ("A" "A")^("B" "A")) with transition defined as ("A" ("A" "A")) ->
	    ;; ("A" ("A" "B"))
	    
	    (in-current-state-but-not-trans (sc.chart::get-states-described-by-name possible-final-state-names
										 in-current-state-but-not-trans))
	    ;; no orthogonal states involved
	    (t possible-final-state-names))))
    (cond
      ((not final-states) (error "Couldn't find final state ?"))
      ;; final states not completely specified, need to select default state
      ((> (length final-states) 1)
       (let ((final-from-partial (sc.chart::get-partial-default-state final-states trans-final-state-name)))
	 (unless final-from-partial
	   (error "Couldn't determine default states for final states."))
	 final-from-partial))
      ;; final state completely specified
      (t (first final-states)))))


(defun get-reentry-actions (final-state transitions)
  (let+ ((states-with-reentry-actions
	  (mapcar #'sc.fsm::initial-state-name
		  (remove-if #'not transitions
			     :key #'(lambda (tr) (sc.key::state= (sc.fsm::initial-state-name tr)
							  (sc.fsm::final-state-name tr))))))
	 (joined-reentry-state (if states-with-reentry-actions
				   (reduce #'sc.key::join states-with-reentry-actions)
				   (return-from get-reentry-actions '())))
	 (explicit-state (sc.chart::remove-implicit-substates final-state joined-reentry-state)))
    (remove-if #'not (recursive-accumulation explicit-state #'sc.chart::on-reentry))))










(defun make-fsm/target (on-entry-actions on-exit-actions on-reentry-actions
			initial-state final-state clauses final-fsm/state)
  (if (and clauses
	   (or (> (length clauses) 1)
	       (typep (car clauses)
		      'sc.chart::guard-clause)))
      (make-instance 'guarded-target
		     :on-entry-actions on-entry-actions
		     :on-exit-actions on-exit-actions
		     :on-reentry-actions on-reentry-actions
		     :initial-name (sc.key::from-chart-state initial-state)
		     :final-name (sc.key::from-chart-state final-state)
 		     :clauses clauses
 		     :state final-fsm/state)
      (make-instance 'target
		     :on-entry-actions on-entry-actions
		     :on-exit-actions on-exit-actions
		     :on-reentry-actions on-reentry-actions
		     :initial-name (sc.key::from-chart-state initial-state)
		     :final-name (sc.key::from-chart-state final-state)
		     :state final-fsm/state)))

(defun set-transition-target (initial-state event-name combined-transitions all-fsm/states all-states)
  (let+ ((initial-fsm/state (find-state-for initial-state all-fsm/states))
	 (final-state (determine-final-states all-states initial-state combined-transitions))
	 (final-fsm/state (find-state-for final-state all-fsm/states))
	 (clauses (mapcar #'clause combined-transitions))
	 ((&values on-exit-actions on-entry-actions)
	  (determine-entry/exit-actions initial-state final-state))
	 ;; assuming that
	 (on-reentry-actions (get-reentry-actions final-state combined-transitions))
	 (target (make-fsm/target on-entry-actions on-exit-actions on-reentry-actions
				  initial-state final-state clauses final-fsm/state))
 	 (target* (assoc event-name (ev->state initial-fsm/state))))
    (if target*
	(setf (cdr target*) (append (cdr target*) (list target)))
	(push (cons event-name (list target)) (ev->state initial-fsm/state)))))



(defun combine-sets (sets)
  (cond
    ((= (length sets) 1)
     (iter
       (for el in (car sets))
       (collect (list el))))
    ((> (length sets) 1)
     (iter outer
       (with set = (car sets))
       (with rest = (combine-sets (cdr sets)))
       (for el in set)
       (iter
	 (for r in rest)
	 (in outer (collect (append (list el) r))))))))




 (defun combine-trans-by-guards (transitions)
  ""
  (let+ (({c.tr}* (mapcar #'(lambda (tr) (mapcar #'(lambda (c) (cons c tr)) (sc.chart::clauses tr)))
			  transitions))
	 
   	 (combined-{c.tr}* (combine-sets {c.tr}*)))
    (mapcar #'(lambda ({c.tr}*)
		(mapcar #'(lambda (c.tr)
			    (let+ (((c . tr) c.tr)
				   ((&slots sc.chart::initial-state-name sc.chart::event-name) tr)
				   ((&slots sc.dsl::final-state) c))
			      (make-instance 'sc.fsm::transition
					     :initial-state-name sc.chart::initial-state-name
					     :final-state-name  sc.dsl::final-state
					     :event-name sc.chart::event-name
					     :clause c)))
			{c.tr}*))
	    combined-{c.tr}*)))




(defun set-transitions-for-states (all-states all-fsm/states all-transitions)
  (labels ((find-events/transition-originating-from-state (state)
	     (group-by:group-by (remove-if-not
				 #'(lambda (tr)
				     (sc.chart::state-described-by-name
				      state (sc.chart::initial-state-name tr)))
				 all-transitions)
				:key #'sc.chart::event-name :value #'identity)))
    (iter 
      (for initial-state in all-states)
      ;; for every EVENT-NAME and TRANSITIONS* originating in INITIAL-STATE
      ;; find and set FINAL-STATE when applying TRANSITIONS* to INITIAL-STATE
      (iter
	(for (event-name . transitions)
	     in (find-events/transition-originating-from-state initial-state))
	(when transitions
	  (for combined-by-guards = (combine-trans-by-guards transitions))
	  (iter
	    (for transitions in combined-by-guards)
	    (set-transition-target initial-state event-name transitions all-fsm/states all-states)))))))




