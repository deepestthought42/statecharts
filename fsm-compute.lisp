(in-package #:sc.fsm)

#+nil
(declaim (optimize (speed 3) (safety 1)))

(defun create-states (states)
  (iter
    (for s in states)
    (for (values name index) = (sc.key::from-chart-state s))
    (for fsm = (make-instance 'sc.fsm::state :name name))
    (setf (sc.chart::fsm-state s) fsm)
    (collect fsm)))





(defun find-state-for (state all-fsm/states)
  (declare (ignore all-fsm/states))
  (sc.chart::fsm-state state))



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
	     (alexandria:mappend #'(lambda (s)
				     (remove-if #'not (recursive-accumulation s accessor)))
				 states)))
    (let+ (((&values in-initial-but-not-final
		     in-final-but-not-initial)
	    (sc.chart::difference initial final)))
      (values (acc in-initial-but-not-final #'sc.chart::on-exit)
	      (acc in-final-but-not-initial #'sc.chart::on-entry)))))



(defun determine-final-states (all-states transitions)
  (let+ ((trans-final-state-name (reduce #'sc.key::join
					 (mapcar #'sc.fsm::final-state-name transitions)))
	 (filter (sc.key::state-bits trans-final-state-name))
	 (final-states
	  (iter
	    (for s in all-states)
	    (for id = (sc.chart::state-bit s))
	    (when (= filter (logand filter id))
	      (collect s)))))

    (cond
      ((not final-states) (error "Couldn't find final state ?"))
      ;; final states not completely specified, need to select default state or
      ;; history states
      ((> (length final-states) 1)
       (let+ (((&values default-state history-states)
	       (sc.chart::resolve-final-state final-states
					      trans-final-state-name)))
	 (unless default-state (error "Couldn't determine default states for final states."))
	 (values default-state history-states)))
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
	 (explicit-state (sc.chart::remove-implicit-substates final-state
							      joined-reentry-state)))
    (remove-if #'not (recursive-accumulation explicit-state #'sc.chart::on-reentry))))



(defun make-fsm/target (on-entry-actions on-exit-actions on-reentry-actions
			initial-state final-state clauses default-final-fsm/state)
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
 		     :state default-final-fsm/state)
      (make-instance 'target
		     :on-entry-actions on-entry-actions
		     :on-exit-actions on-exit-actions
		     :on-reentry-actions on-reentry-actions
		     :initial-name (sc.key::from-chart-state initial-state)
		     :final-name (sc.key::from-chart-state final-state)
		     :state default-final-fsm/state)))


(defun set-history-updaters (target history-fsm-states)
  (mapcar #'(lambda (state) (push target (targets-with-history state)))
	  history-fsm-states))

(defun remove-guarded (combined-transitions initial-state-name)
  (remove-if #'(lambda (tr)
		 (let+ (((&slots clause) tr))
		   (when (typep clause 'sc.dsl::when-in-state-clause)
		     (sc.key::difference (sc.dsl::in-state clause)
					 initial-state-name
					 :accept-unspecified-substate t))))
	     combined-transitions))

(defun set-transition-target (initial-chart-s event-name combined-transitions
			      all-fsm/states all-chart-states)
  (let+ ((initial-fsm/state (find-state-for initial-chart-s all-fsm/states))
	 ;; since we work on state-names, create one for INITIAL-STATE
	 (initial-state-name (sc.key::from-chart-state initial-chart-s))
	 (transitions (remove-guarded combined-transitions initial-state-name))
	 ;; transitions could have been cleared at this point
	 (nil (when (not transitions) (return-from set-transition-target)))
	 ((&values default-final-state history-states)
	  (determine-final-states all-chart-states transitions))
	 (default-final-fsm/state (find-state-for default-final-state all-fsm/states))
	 (clauses (mapcar #'clause transitions))
	 ((&values on-exit-actions on-entry-actions)
	  (determine-entry/exit-actions initial-chart-s default-final-state))
	 (on-reentry-actions (get-reentry-actions default-final-state transitions))
	 (target (make-fsm/target on-entry-actions on-exit-actions on-reentry-actions
				  initial-chart-s default-final-state
				  clauses default-final-fsm/state))
	 (target* (assoc event-name (ev->state initial-fsm/state)))
	 (history-fsm-states (mapcar #'(lambda (s) (find-state-for s all-fsm/states))
				     history-states)))
    (set-history-updaters target history-fsm-states)
    (if target*
	(setf (cdr target*) (append (cdr target*) (list target)))
	(push (cons event-name (list target)) (ev->state initial-fsm/state)))))







(defun combine-trans-by-guards (transitions &optional state-id-only)
  ""
  (let+ ((c.tr (mapcar #'(lambda (tr) (mapcar #'(lambda (c) (cons c tr)) (sc.chart::clauses tr)))
			  transitions))
	 
   	 (combined-c.tr (sc.utils::combine-sets c.tr)))
    (mapcar #'(lambda (c.tr)
		(mapcar #'(lambda (c.tr)
			    (let+ (((c . tr) c.tr)
				   ((&slots sc.chart::initial-state-name sc.chart::event-name) tr)
				   ((&slots sc.dsl::final-state) c))
			      (make-instance 'sc.fsm::transition
					     :initial-state-name sc.chart::initial-state-name
					     :final-state-name
					     (if state-id-only
						 (make-instance 'sc.key::state-id
								:defining-element
								(sc.key::defining-element sc.dsl::final-state)
								:state-bits (sc.key::state-bits sc.dsl::final-state)
								:excluded-state-bits
								(sc.key::excluded-state-bits sc.dsl::final-state))
						 sc.dsl::final-state)
					     :event-name sc.chart::event-name
					     :clause c)))
			c.tr))
	    combined-c.tr)))





(defun find-events/transition-originating-from-state (state all-transitions)
  (group-by:group-by (remove-if-not
		      #'(lambda (tr)
			  (sc.chart::key-subset-of-state
			   state (sc.chart::initial-state-name tr)))
		      all-transitions)
		     :key #'sc.chart::event-name :value #'identity))

(defun set-transitions-for-states (all-states all-fsm/states all-transitions)
  (iter
    (for initial-state in all-states)
    ;; for every EVENT-NAME and TRANSITIONS* originating in INITIAL-STATE
    ;; find and set FINAL-STATE when applying TRANSITIONS* to INITIAL-STATE
    (iter
      (for (event-name . transitions)
	   in (find-events/transition-originating-from-state initial-state all-transitions))
      (when transitions
	(for combined-by-guards = (combine-trans-by-guards transitions t))
	(iter
	  (for transitions in combined-by-guards)
	  (set-transition-target initial-state event-name
				 transitions all-fsm/states all-states))))))





