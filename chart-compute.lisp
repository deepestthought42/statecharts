(in-package #:statecharts.chart)

(define-condition invalid-chart (error)
  ((reason :accessor reason :initarg :reason
	   :initform ""))
  (:report (lambda (c stream)
	     (format stream "Invalid chart definition: ~a" (reason c)))))


(defgeneric compute-substates (s))

(defmethod compute-substates ((s t))
  '())



(defmethod compute-substates ((s sc.dsl::state))
  (list (make-instance 's :name (sc.dsl::name s)
			  :defining-state s
			  :state-bit (sc.dsl::state-bit s)
			  :on-entry (sc.dsl::on-entry s)
			  :on-reentry (sc.dsl::on-reentry s)
			  :on-exit (sc.dsl::on-exit s))))


(defun get-states (elements &optional default-name)
  (iter
    (with default-state = nil)
    (for e in elements)
    (if (typep e 'sc.dsl::state)
	(progn
	  (collect e into states)
	  (if (equal (sc.dsl::name e) default-name)
	      (setf default-state e))))
    (finally (return (values states default-state)))))

(defun sub-states-check (sub-states name &optional (cluster-desc "CLUSTER"))
  (cond
    ((not sub-states)
     (error 'invalid-chart :reason (format nil "~a: >~a< has no sub-states."
					   cluster-desc name)))
    ((= 1 (length sub-states))
     (error 'invalid-chart :reason (format nil "~a: >~a< has only one sub-state."
					   cluster-desc name)))))

(defmethod compute-substates ((cluster sc.dsl::cluster))
  (let+ (((&slots sc.dsl::name sc.dsl::elements sc.dsl::default-state
		  sc.dsl::selector-type)
	  cluster)
	 ((&values states default-state) (get-states sc.dsl::elements sc.dsl::default-state))
	 (state-bit (sc.dsl::state-bit cluster))
	 (default-state-bit (sc.dsl::state-bit default-state)))
    (sub-states-check states sc.dsl::name)
    (iter outer
      (for e in states)
      (for unsorted-sub-states = (compute-substates e))
      (for sub-states = (sort unsorted-sub-states #'string< :key #'name))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance (case sc.dsl::selector-type
				      (sc::h 'history-s-xor)
				      (sc::d 's-xor)
				      (t (error "Unknown selector type: ~a"
						sc.dsl::selector-type)))
				    :name sc.dsl::name
				    :sub-state s
				    :defining-state cluster
				    :state-bit (logior state-bit (state-bit s))
				    :default-state-bit (logior (default-state-bit s)
							       default-state-bit)
				    :on-entry (sc.dsl::on-entry cluster)
				    :on-reentry (sc.dsl::on-reentry cluster)
				    :on-exit (sc.dsl::on-exit cluster)
				    :default-state sc.dsl::default-state)))))))




(defmethod compute-substates ((ortho sc.dsl::orthogonal))
  (let+ (((&slots sc.dsl::name sc.dsl::elements) ortho)
	 (states (get-states sc.dsl::elements))
	 (state-bit (sc.dsl::state-bit ortho))
	 (substates
	  (iter
	    (for e in sc.dsl::elements)
	    (for sub-states = (compute-substates e))
	    (when sub-states (collect sub-states)))))
    (sub-states-check states sc.dsl::name "ORTHOGONAL CLUSTER")
    (alexandria:when-let (non-clusters (remove-if #'(lambda (s) (typep s 'sc.dsl::cluster)) states))
      (error 'invalid-chart
	     :reason (format nil "ORTHOGONAL CLUSTER: >~a< has non-cluster sub-state(s): ~{~a ~}"
			     sc.dsl::name non-clusters)))
    (iter
      (for sub-states in (sc.utils::combine-sets substates))
      (collect (make-instance 's-and :name sc.dsl::name
				     :defining-state ortho
				     :state-bit
				     (reduce #'logior sub-states :key #'state-bit
								 :initial-value state-bit)
				     :default-state-bit
				     (reduce #'logior sub-states :key #'default-state-bit
								 :initial-value state-bit)
				     :on-entry (sc.dsl::on-entry ortho)
				     :on-reentry (sc.dsl::on-reentry ortho)
				     :on-exit (sc.dsl::on-exit ortho)
				     :sub-states (sort sub-states #'string< :key #'name))))))


;;;; lookup



(defgeneric calculate-lookup (state defined-in))



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
				       chart-element super-state t))
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

