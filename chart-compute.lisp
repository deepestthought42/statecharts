(in-package #:statecharts.chart)


;;; parse statecharts

(defgeneric compute-substates (s))

(defmethod compute-substates ((s t)) '())

(defmethod compute-substates ((s dsl::state))
  (list (make-instance 's :name (dsl::name s)
			  :defining-state s
			  :on-entry (dsl::on-entry s)
			  :on-reentry (dsl::on-reentry s)
			  :on-exit (dsl::on-exit s))))

(defmethod compute-substates ((cluster dsl::cluster))
  (let+ (((&slots dsl::name dsl::elements dsl::default-state) cluster))
    (iter outer
      (for e in dsl::elements)
      (for sub-states = (compute-substates e))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance 's-xor :name dsl::name :sub-state s
					   :defining-state cluster
					   :on-entry (dsl::on-entry cluster)
					   :on-reentry (dsl::on-reentry cluster)
					   :on-exit (dsl::on-exit cluster)
					   :default-state dsl::default-state)))))))

(defmethod compute-substates ((ortho dsl::orthogonal))
  (let+ (((&slots dsl::name dsl::elements) ortho)
	 (lst-of-lst-of-substates
	  (remove-if #'not
		     (mapcar #'(lambda (s) (compute-substates s)) dsl::elements))))
    (labels ((combine-elements (super-lst) 
	       (cond
		 ((not (cdr super-lst))
		  (mapcar #'(lambda (s)
			      (make-instance 's-and :sub-states (list s)
						    :defining-state ortho
						    :on-entry (dsl::on-entry ortho)
						    :on-reentry (dsl::on-reentry ortho)
						    :on-exit (dsl::on-exit ortho)
						    :name dsl::name))
			  (car super-lst)))
		 (t
		  (let ((states-1 (car super-lst))
			(states-n (combine-elements (rest super-lst))))
		    (iter
		      (for s-1 in states-1)
		      (appending
		       (mapcar #'(lambda (s-n)
				   (make-instance 's-and :name dsl::name
							 :defining-state ortho
							 :on-entry (dsl::on-entry ortho)
							 :on-reentry (dsl::on-reentry ortho)
							 :on-exit (dsl::on-exit ortho)
							 :sub-states
							 (append (list s-1)
								 (sub-states s-n))))
			       states-n))))))))
      (combine-elements lst-of-lst-of-substates))))


;;; transitions


(defmethod compute-transitions ((s t) super-state  dsl-element) '())


(defun %make-clause (clause chart-element super-state)
  (let+ ((fs-description (dsl::final-state clause))
	 (fs (name::from-description fs-description chart-element super-state)))
    (sc::copy-object clause :final-state fs)))


(defun %make-transitions (elements super-state chart-element)
  (iter outer
    (for el in elements)
    (when (typep el 'dsl::transition)
      (for initial-state-name
	   = (name::from-description (dsl::initial-state el) chart-element super-state))
      (for clauses
	   = (mapcar #'(lambda (c) (%make-clause c chart-element super-state)) (dsl::clauses el)))
      (collect
	  (make-instance 'chart::transition :event-name (dsl::event-symbol el)
					    :initial-state-name initial-state-name
					    :transition-group-id (dsl::id el)
					    :clauses clauses)))))


(defmethod compute-transitions ((s dsl::cluster) super-state  dsl-element)
  (let+ (((&slots dsl::name dsl::elements) s)
	 (super-state (append super-state (list dsl::name)))
	 (transitions-for-sub-states
	  (iter
	    (for el in dsl::elements)
	    (appending (compute-transitions el super-state  dsl-element)))))
    (append transitions-for-sub-states
	    (%make-transitions dsl::elements super-state  dsl-element))))

(defmethod compute-transitions ((s dsl::orthogonal) super-state  dsl-element)
  (let+ (((&slots dsl::name dsl::elements) s)
	 (super-state (append super-state (list dsl::name)))
	 (transitions-for-sub-states
	  (iter
	    (for el in dsl::elements)
	    (appending (compute-transitions el super-state  dsl-element)))))
    (append transitions-for-sub-states
	    (%make-transitions dsl::elements super-state  dsl-element))))


;;; accumulate events

(defun gather-events-from-transitions (transitions)
  (let+ ((table (make-hash-table :test 'equal)))
    (map nil #'(lambda (tr)
		 (if (gethash (event-name tr) table)
		     (push tr (gethash (event-name tr) table))
		     (setf (gethash (event-name tr) table) (list tr))))
	 transitions)
    table))

