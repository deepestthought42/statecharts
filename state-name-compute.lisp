(in-package #:statecharts.state-key)

;; the dsl to address states is simple:
;; - "A" references the state with name "A" in the current superstate
;;  
;; - ("H" "A") references the the substate "A" of state "H" within
;;   the current superstate
;; - ("H" ("A" "B") ("C" "D")) references  the substate(s) of
;;   the orthogonal cluster "H"; since a substate of an orthogonal cluster is always 
;;   active, just the name of the substate (e.g. "A") is nonsensical


;; fixmee: should have done this with pattern matching ... 

(defun throw-invalid (state-description reason args)
  (error 'sc.cond::invalid-state-descriptor
	 :descriptor state-description
	 :reason (apply #'format nil reason args)))


(defun make-and-state-name (state element states-description)
  (labels ((%throw (reason &rest args) (%throw states-description reason args)))
    (let+ ((state-elements (filter-state-elements (sc.dsl::elements element)))
	   ((&values sub-states sub-state-names)
	    (cond
	      ;; multiple sub-states
	      ((listp (first states-description))
	       (iter
		 (for sub-state in states-description)
		 (for s-name in (first sub-state))
		 (when (not (symbolp s-name))
		   (%throw "State name not a string: ~a" s-name))
		 (collect
		     (make-state-name
		      sub-state
		      (find-element s-name state-elements))
		   into sub-states)
		 (collect s-name into sub-state-names)
		 (finally (return (values sub-states sub-state-names)))))
	      ;; direct sub-state
	      ((symbolp (first states-description))
	       (values (list (make-state-name
			      states-description
			      (find-element (first states-description)
					    state-elements)))
		       (list (first states-description))))))
	   (no-duplicates
	    (remove-duplicates sub-states :test #'eq :key #'name))
	   ((&values state-bits-covered state-bits-unspecified)
	    (iter
	      (for element in state-elements)
	      (if (find (sc.dsl::name element) sub-state-names :test #'equal)
		  (reducing (sc.dsl::sub-states-bits element) by #'logior into specified)
		  (reducing (sc.dsl::sub-states-bits element) by #'logior into unspecified))
	      (finally (return (values (if specified specified 0)
				       (if unspecified unspecified 0))))))
	   (state-bits-unspecified (reduce #'logior sub-states
					   :key #'state-bits-unspecified
					   :initial-value state-bits-unspecified))
	   (state-bits-covered (logior state-bits-covered (sc.dsl::state-bit element))))
      (if (not (= (length sub-states)
		  (length no-duplicates)))
	  (%throw "Found duplicate substate definitions for: ~a"
		  (mapcar #'name (set-difference sub-states no-duplicates))))
      (make-instance 'and-state :name state
				:defining-element element
				:state-bits-covered state-bits-covered
				:state-bits-unspecified state-bits-unspecified
				:state-bits
				(reduce #'logior sub-states
					:key #'state-bits
					:initial-value (sc.dsl::state-bit element)) 
				:sub-states sub-states))))

(defun filter-state-elements (elements)
  (labels ((state-p (obj) (typep obj 'sc.dsl::state)))
    (remove-if-not #'state-p elements)))

(defun find-element (state state-elements)
  (alexandria:if-let (sub-s (find state state-elements
				  :key #'sc.dsl::name
				  :test #'eq))
    sub-s (throw-invalid state "Could not find state with name: ~a" state)))

(defun make-state-name (state-description element)
  (labels ((%throw (reason &rest args) (%throw state-description reason args)))
    (let+ (((state &rest rest) state-description))
      (cond
	((not state-description) nil)
	((not (and (symbolp state)
		   (eq state (sc.dsl::name element))))
	 (%throw "Unknown or invalid state name: ~a" state))
	;; orthogonal cluster
	((and (typep element 'sc.dsl::orthogonal))
	 (make-and-state-name state element rest))
	;; cluster state
	((and (typep element 'sc.dsl::cluster)
	      (not (typep element 'sc.dsl::orthogonal)))
	 (let ((sub-state (cond
			    ((not (first rest)) nil)
			    ((symbolp (first rest))
			     (make-state-name
			      rest (find-element (first rest)
						 (sc.dsl::elements element))))
			    ((stringp (first rest))
			     (make-state-name
			      rest (find-element (alexandria:make-keyword (first rest))
						 (sc.dsl::elements element))))
			    (t
			     (%throw "Invalid state syntax: ~a"
				     (first rest))))))
	   (make-instance 'or-state
			  :name state
	 		  :defining-element element
			  :state-bits-covered (logior (sc.dsl::sub-states-bits element)
						      (sc.dsl::state-bit element))
			  :state-bits-unspecified (if sub-state
						      (state-bits-unspecified sub-state)
						      (sc.dsl::sub-states-bits element))
			  :state-bits (logior (sc.dsl::state-bit element)
					      (if sub-state (state-bits sub-state) 0))
			  :sub-state sub-state)))
	;; leaf state
	((and (typep element 'sc.dsl::state)
	      (not (typep element 'sc.dsl::cluster)))
	 (make-instance 'state :name state
			       :defining-element element
			       :state-bits-covered (sc.dsl::state-bit element)
			       :state-bits-unspecified 0
			       :state-bits (sc.dsl::state-bit element)))
	;;
	(t (%throw "Couldn't parse key: ~a" state-description))))))

(defun from-description (state-description dsl-element &optional super-state id-only)
  (labels ((to-symbols (params)
	     (mapcar #'alexandria:make-keyword params))
	   (%throw (reason &rest args) (%throw state-description reason args)))
    (let ((state-name
	    (cond
	      ((and (listp state-description)
		    (equal (first state-description) :/))
	       (make-state-name (to-symbols (rest state-description)) dsl-element))
	      ((and super-state (listp state-description))
	       (make-state-name (to-symbols (append super-state state-description)) dsl-element))
	      ((listp state-description)
	       (make-state-name (to-symbols (append super-state state-description)) dsl-element))
	      (t
	       (make-state-name (to-symbols
				 (append super-state (list state-description)))
				dsl-element)))))
      (if id-only
	  (make-instance 'state-id
			 :state-bits (state-bits state-name)
			 :state-bits-covered (state-bits-covered state-name)
			 :state-bits-unspecified (state-bits-unspecified state-name)
			 :defining-element dsl-element)
	  state-name))))



;;; copy state-names
(defgeneric copy (state))

(defmethod copy ((s state-id))
  (make-instance 'state-d :state-bits (state-bits s)
			  :state-bits-unspecified (state-bits-unspecified s)
			  :state-bits-covered (state-bits-covered s)
			  :defining-element (defining-element s)))

(defmethod copy ((sn state))
  (make-instance 'state :name (name sn)
			:state-bits (state-bits sn)
			:state-bits-unspecified (state-bits-unspecified sn)
			:state-bits-covered (state-bits-covered sn)
			:defining-element (defining-element sn)))

(defmethod copy ((sn or-state))
  (make-instance 'or-state
		 :name (name sn)
		 :state-bits (state-bits sn)
		 :state-bits-unspecified (state-bits-unspecified sn)
		 :state-bits-covered (state-bits-covered sn)
		 :defining-element (defining-element sn)
		 :sub-state
		 (if (sub-state sn)
		     (copy (sub-state sn)))))

(defmethod copy ((sn and-state))
  (make-instance 'and-state
		 :name (name sn)
		 :defining-element (defining-element sn)
		 :state-bits-unspecified (state-bits-unspecified sn)
		 :state-bits-covered (state-bits-covered sn)
		 :state-bits (state-bits sn)
		 :sub-states
		 (mapcar #'copy (sub-states sn))))




(defmethod join ((a state-id) (b state-id))
  (make-instance 'state-id :defining-element (defining-element a)
			   :state-bits-covered (logior (state-bits-covered a)
						       (state-bits-covered b))
			   :state-bits-unspecified (logand (state-bits-unspecified a)
							(state-bits-unspecified b))
			   :state-bits (logior (state-bits a)
					       (state-bits b))))



(defmethod intersect-state-names ((a state-id) (b state-id))
  (make-instance 'state-id :defining-element (defining-element a)
			   :state-bits-covered (logand (state-bits-covered a)
						       (state-bits-covered b))
			   :state-bits-unspecified (logxor (state-bits-unspecified a)
							(state-bits-unspecified b))
			   :state-bits (logand (state-bits a)
					       (state-bits b))))



(defgeneric difference (a b &key accept-unspecified-substate)
  (:method (a b &key accept-unspecified-substate)
    (declare (ignore b accept-unspecified-substate))
    (copy a))
  (:documentation "Return the part (and/or) of key A that is not present in key
B.  When ACCEPT-UNSPECIFIED-SUBSTATE is true, unspecified substates in a cluster
are ignored, such that: (difference (a b) (a)) -> NIL."))


(defmethod difference ((a state-id) (b state-id) &key accept-unspecified-substate)
  (declare (ignore accept-unspecified-substate))
  (make-instance 'state-id :defining-element (defining-element a)
			   :state-bits (logandc2 (state-bits a)
						 (state-bits b))
			   :state-bits-covered (logandc2 (state-bits-covered a)
							 (state-bits-covered b))
			   :state-bits-unspecified (logandc2 (state-bits-unspecified a)
							  (state-bits-unspecified b))))


;;; compare state names

(defgeneric state= (a b)
  (:documentation "Returns true for objects A, B of type STATE-NAME that are same."))

;; (defmethod state= ((a state) (b t)) nil)
;; (defmethod state= ((a t) (b state)) nil)


(defmethod state= ((a state-id) (b state-id))
  (= (state-bits a)
     (state-bits b)))


;;; find dsl-object for state
(defgeneric find-dsl-object (state dsl-object)
  (:documentation "Given a state in STATE-NAME, will return the branch of DSL-OBJECT
  that corresponds to STATE-NAME. Returns NIL if it cannot find a dsl-object that
  correpsonds to STATE-NAME.")
  (:method ((state t) (dsl-object t)) nil))




(defmethod find-dsl-object ((state state) (dsl-object sc.dsl::state))
  (when (eq (name state) (sc.dsl::name dsl-object)) dsl-object))

;;; from-chart-state state-name from s


(defgeneric from-chart-state (s))

(defmethod from-chart-state ((s sc.chart::s))
  (make-instance 'sc.key::state-id :defining-element (sc.chart::defining-state s)
				   :state-bits (sc.chart::state-bit s)
				   :state-bits-covered (logior (sc.chart::state-bit s)
							       (typecase s
								 (sc.chart::s-xor (sc.chart::sub-state-bit s))
								 (sc.chart::s-and (sc.chart::sub-states-bits s))
								 (t 0)))
				   
				   :state-bits-unspecified 0))

;; printing is defined here instead of in state-name-compute to have
;; all packages (in particular sc.dsl) and functions available



(defmethod print-object ((obj state-id) stream)
  (print-unreadable-object (obj stream)
    (sc.utils::%print-object obj stream)))

(defmethod sc.utils::%print-object ((obj state-id) stream)
  (format stream "key [~a]: ~a"
	  (sc.utils::integer->bit-vector (state-bits obj))
	  (%print-key (defining-element obj) (state-bits obj))))


(defun %shall-print (defining-element id)
  (and (typep defining-element 'sc.dsl::state)
       (> (sc.dsl::state-bit defining-element) 0)
       (> (logand (sc.dsl::state-bit defining-element) id)
	  0)))


(defmethod %print-key ((defining-element sc.dsl::state) id)
  (if (%shall-print defining-element id)
      (values (format nil "(~a)" (sc.dsl::name defining-element))
	      t)))

(defmethod %print-key ((defining-element sc.dsl::cluster) id)
  (let+ ((elements (remove-if-not #'(lambda (e) (typep e 'sc.dsl::state))
				  (sc.dsl::elements defining-element)))
	 ((&values sub-string printed-sub)
	  (iter
	    (for e in elements)
	    (for (values sub-string printed) = (%print-key e id))
	    (when printed (return (values sub-string printed))))))
    (cond
      (printed-sub
       (values (format nil "(~a ~a)"
		       (sc.dsl::name defining-element)
		       sub-string)
	       t))
      ((%shall-print defining-element id)
       (values (format nil "(~a)"
		       (sc.dsl::name defining-element))
	       t)))))

(defmethod %print-key ((defining-element sc.dsl::orthogonal) id)
  (let+ (((&values sub-strings printed)
	   (iter
	     (for e in (sc.dsl::elements defining-element))
	     (unless (typep e 'sc.dsl::state)
	       (next-iteration))
	     (for (values sub-string printed) = (%print-key e id))
	     (when printed
	       (collect sub-string into sub-strings))
	     (finally
	      (return
		(when sub-strings (values sub-strings t)))))))
    (cond
      (printed
       (values (format nil "(~a ~{~a~^^~})"
		       (sc.dsl::name defining-element)
		       sub-strings)
	       t))
      ((%shall-print defining-element id)
       (values (format nil "(~a)" (sc.dsl::name defining-element))
	       t)))))

