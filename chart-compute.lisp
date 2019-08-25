(in-package #:statecharts.chart)


;;; parse statecharts


(defgeneric compute-substates (s &optional id))

(defmethod compute-substates ((s t) &optional id)
  (declare (ignore id))
  '())

(defmethod compute-substates ((s sc.dsl::state) &optional (id 0))
  (list (make-instance 's :name (sc.dsl::name s)
			  :id id
			  :defining-state s
			  :on-entry (sc.dsl::on-entry s)
			  :on-reentry (sc.dsl::on-reentry s)
			  :on-exit (sc.dsl::on-exit s))))


(defmethod compute-substates ((cluster sc.dsl::cluster) &optional (id 0))
  (let+ (((&slots sc.dsl::name sc.dsl::elements sc.dsl::default-state
		  sc.dsl::selector-type) cluster)
	 (no-of-substates 0))
    (iter outer
      (with i = 0)
      (for e in sc.dsl::elements)
      (for sub-states = (sort (compute-substates e i) #'string< :key #'name))
      (when sub-states (incf i) (incf no-of-substates))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance (case sc.dsl::selector-type
				      (sc::h 'history-s-xor)
				      (sc::d 's-xor)
				      (t (error "Unknown selector type: ~a"
						sc.dsl::selector-type)))
 				    :name sc.dsl::name :sub-state s
				    :id id
				    :no-of-substates no-of-substates
				    :defining-state cluster 
				    :on-entry (sc.dsl::on-entry cluster)
				    :on-reentry (sc.dsl::on-reentry cluster)
				    :on-exit (sc.dsl::on-exit cluster)
				    :default-state sc.dsl::default-state)))))))




(defmethod compute-substates ((ortho sc.dsl::orthogonal) &optional (id 0))
  (let+ (((&slots sc.dsl::name sc.dsl::elements) ortho)
	 (no-of-substates 0)
	 (set-of-substates
	  (iter
	    (with i = 0)
	    (for e in sc.dsl::elements)
	    (for sub-states =  (compute-substates e i))
	    (when sub-states
	      (incf i) (incf no-of-substates)
	      (collect sub-states))))
	 (combined-substates (sc.utils::combine-sets set-of-substates)))
    (iter
      (for sub-states in combined-substates)
      (collect (make-instance 's-and :name sc.dsl::name
				     :id id
				     :no-of-substates no-of-substates
				     :defining-state ortho
				     :on-entry (sc.dsl::on-entry ortho)
				     :on-reentry (sc.dsl::on-reentry ortho)
				     :on-exit (sc.dsl::on-exit ortho)
				     :sub-states (sort sub-states #'string< :key #'name))))))


;;; transitions


(defmethod compute-transitions ((s t) super-state  dsl-element) '())


(defgeneric %make-clause (clause chart-element super-state))

(defmethod %make-clause ((clause sc.dsl::transition-clause) chart-element super-state)
  (let+ ((fs-description (sc.dsl::final-state clause))
	 (fs (sc.key::from-description fs-description chart-element super-state)))
    (sc.utils::copy-object clause :final-state fs)))

(defmethod %make-clause ((clause sc.dsl::when-in-state-clause) chart-element super-state)
  (let+ ((fs-description (sc.dsl::final-state clause))
	 (in-state-description (sc.dsl::in-state clause))
	 (fs (sc.key::from-description fs-description chart-element super-state))
	 (is (sc.key::from-description in-state-description chart-element super-state)))
    (sc.utils::copy-object clause :final-state fs :in-state is)))

(defun %make-transitions (elements super-state chart-element)
  (iter outer
    (for el in elements)
    (when (typep el 'sc.dsl::transition)
      (for initial-state-name
	   = (sc.key::from-description (sc.dsl::initial-state el)
				       chart-element super-state))
      (for clauses
	   = (mapcar #'(lambda (c) (%make-clause c chart-element super-state))
		     (sc.dsl::clauses el)))
      (collect
	  (make-instance 'sc.chart::transition :event-name (sc.dsl::event-symbol el)
					       :initial-state-name initial-state-name
					       :clauses clauses)))))


(defmethod compute-transitions ((s sc.dsl::state-with-substates) super-state  dsl-element)
  (let+ (((&slots sc.dsl::name sc.dsl::elements) s)
	 (super-state (append super-state (list sc.dsl::name)))
	 (transitions-for-sub-states
	  (iter
	    (for el in sc.dsl::elements)
	    (appending (compute-transitions el super-state  dsl-element)))))
    (append transitions-for-sub-states
	    (%make-transitions sc.dsl::elements super-state  dsl-element))))



;;; accumulate events

(defun gather-events-from-transitions (transitions)
  (let+ ((table (make-hash-table :test 'equal)))
    (map nil #'(lambda (tr)
		 (if (gethash (event-name tr) table)
		     (push tr (gethash (event-name tr) table))
		     (setf (gethash (event-name tr) table) (list tr))))
	 transitions)
    table))

