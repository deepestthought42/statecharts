(in-package #:statecharts.chart)


;;; parse statecharts

(defgeneric compute-substates (s))

(defmethod compute-substates ((s t)) '())

(defmethod compute-substates ((s sc.dsl::state))
  (list (make-instance 's :name (sc.dsl::name s)
			  :defining-state s
			  :on-entry (sc.dsl::on-entry s)
			  :on-reentry (sc.dsl::on-reentry s)
			  :on-exit (sc.dsl::on-exit s))))

(defmethod compute-substates ((cluster sc.dsl::cluster))
  (let+ (((&slots sc.dsl::name sc.dsl::elements sc.dsl::default-state) cluster))
    (iter outer
      (for e in sc.dsl::elements)
      (for sub-states = (compute-substates e))
      (iter
	(for s in sub-states)
	(in outer
	    (collect (make-instance 's-xor :name sc.dsl::name :sub-state s
					   :defining-state cluster
					   :on-entry (sc.dsl::on-entry cluster)
					   :on-reentry (sc.dsl::on-reentry cluster)
					   :on-exit (sc.dsl::on-exit cluster)
					   :default-state sc.dsl::default-state)))))))

(defmethod compute-substates ((ortho sc.dsl::orthogonal))
  (let+ (((&slots sc.dsl::name sc.dsl::elements) ortho)
	 (lst-of-lst-of-substates
	  (remove-if #'not
		     (mapcar #'(lambda (s) (compute-substates s)) sc.dsl::elements))))
    (labels ((combine-elements (super-lst) 
	       (cond
		 ((not (cdr super-lst))
		  (mapcar #'(lambda (s)
			      (make-instance 's-and :sub-states (list s)
						    :defining-state ortho
						    :on-entry (sc.dsl::on-entry ortho)
						    :on-reentry (sc.dsl::on-reentry ortho)
						    :on-exit (sc.dsl::on-exit ortho)
						    :name sc.dsl::name))
			  (car super-lst)))
		 (t
		  (let ((states-1 (car super-lst))
			(states-n (combine-elements (rest super-lst))))
		    (iter
		      (for s-1 in states-1)
		      (appending
		       (mapcar
			#'(lambda (s-n)
			    (make-instance 's-and :name sc.dsl::name
						  :defining-state ortho
						  :on-entry (sc.dsl::on-entry ortho)
						  :on-reentry (sc.dsl::on-reentry ortho)
						  :on-exit (sc.dsl::on-exit ortho)
						  :sub-states
						  (append (list s-1)
							  (sub-states s-n))))
			states-n))))))))
      (combine-elements lst-of-lst-of-substates))))


;;; transitions


(defmethod compute-transitions ((s t) super-state  dsl-element) '())


(defun %make-clause (clause chart-element super-state)
  (let+ ((fs-description (sc.dsl::final-state clause))
	 (fs (sc.key::from-description fs-description chart-element super-state)))
    (sc.utils::copy-object clause :final-state fs)))


(defun %make-transitions (elements super-state chart-element)
  (iter outer
    (for el in elements)
    (when (typep el 'sc.dsl::transition)
      (for initial-state-name
	   = (sc.key::from-description (sc.dsl::initial-state el) chart-element super-state))
      (for clauses
	   = (mapcar #'(lambda (c) (%make-clause c chart-element super-state)) (sc.dsl::clauses el)))
      (collect
	  (make-instance 'sc.chart::transition :event-name (sc.dsl::event-symbol el)
					    :initial-state-name initial-state-name
					    :clauses clauses)))))


(defmethod compute-transitions ((s sc.dsl::cluster) super-state  dsl-element)
  (let+ (((&slots sc.dsl::name sc.dsl::elements) s)
	 (super-state (append super-state (list sc.dsl::name)))
	 (transitions-for-sub-states
	  (iter
	    (for el in sc.dsl::elements)
	    (appending (compute-transitions el super-state  dsl-element)))))
    (append transitions-for-sub-states
	    (%make-transitions sc.dsl::elements super-state  dsl-element))))

(defmethod compute-transitions ((s sc.dsl::orthogonal) super-state  dsl-element)
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

