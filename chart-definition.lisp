(in-package #:statecharts)

(defun %register-event (statechart event-name transition-object)
  (let+ (((&slots events) statechart)
	 (event (alexandria:if-let (existing (gethash event-name events))
		  existing
		  (setf (gethash event-name events)
			(make-instance 'event :name event-name)))))
    (push transition-object (registered-transitions event))))





(defun %c (name description entry exit selector elements)
  (make-instance 'cluster :name name :description description
			  :on-entry (if entry entry (constantly t))
			  :on-exit (if exit exit (constantly t))
			  :selector selector
			  :elements elements))

(defun %o (name description entry exit selector elements)
  (make-instance 'orthogonal :name name :description description
			     :on-entry (if entry entry (constantly t))
			     :on-exit (if exit exit (constantly t))
			     :selector selector
			     :elements elements))

(defun %t (initial-key event final-key if)
  (make-instance 'transition
		 :name (format nil "~a" (append initial-key '(->) final-key))
		 :event event
		 :initial-state initial-key
		 :final-state final-key
		 :guard (if if if (constantly t))))


(defun %s (name description entry exit selector)
  (make-instance 'state :name name :description description
			:on-entry (if entry entry (constantly t))
			:selector selector
			:on-exit (if exit exit (constantly t))))



(defun %dereference-key (superstate key)
  (labels ((valid-descriptor (desc)
	     (unless (stringp desc)
	       (error 'invalid-state-descriptor
		      :descriptor desc))))
    (cond
      ;; (/ "A" "B" "C") references with respect to the root of the tree:
      ;; -> "C" within "B" within "A"
      ((and (listp key) (equal '/ (first key))
	    (mapcar #'valid-descriptor (cdr key)))
       (cdr key))
      ;; ("A" "B") references within the current super state:
      ;; "B" within "A" within the current superstate
      ((and (listp key)
	    (mapcar #'valid-descriptor key)
	    (or (equal nil superstate)
		(mapcar #'valid-descriptor superstate)))
       (append superstate key))
      ;; "A" references "A" within the current superstate
      ((stringp key)
       (append superstate (list key)))
      (t (error 'invalid-state-descriptor
		:descriptor key)))))



(defun %register-state (statechart superstate state-name state-object)
  (let+ (((&slots states) statechart)
	 (key (%dereference-key superstate state-name))
	 (existing (gethash key states)))
    (if existing (error 'state-exists :key key))
    (setf (gethash key states) state-object)
    state-object))


(defun %register-transition (statechart superstate initial-key final-key transition-object)
  (let+ (((&slots transitions) statechart)
	 (key (append initial-key final-key))
	 (existing (gethash key transitions)))
    (if existing (error 'transition-exists :key key))
    (setf (gethash key transitions) transition-object))
  transition-object)



(defun %check-defstatechart-arguments (name description definitions)
  (declare (ignore definitions))
  (cond
    ((not (symbolp name))
     (error 'invalid-chart-syntax
	    :message "Name needs to be a symbol."
	    :offending-code name))
    ((not (stringp description))
     (error 'invalid-chart-syntax
	    :message "Description needs to be a string."
	    :offending-code description))))




(defmacro defstatechart ((name &key (description "")) &body definitions)
  (%check-defstatechart-arguments name description definitions)
  `(let* ((statechart (make-instance 'statecharts::statechart :name ,(string name)
							      :description ,description))
	  (superstate-key '())
	  (current-selector))
     (macrolet ((:d (name &body body)
		  `(let* ((current-selector (make-instance 'statecharts::default-selector
							   :selected-state ,name)))
		     (progn ,@body)))
		(:h (name &body body)
		  `(let* ((current-selector (make-instance 'statecharts::history-selector
							   :selected-state ,name)))
		     (progn ,@body)))
		(:c ((name &key (description "") entry exit) &body sub-states)
		  `(statecharts::%register-state statechart superstate-key ,name 
						 (let ((superstate-key (append superstate-key (list ,name))))
						   (%c ,name ,description statechart
						       superstate-key current-selector
						       (list ,@sub-states)))))
		(:o ((name &key (description "") entry exit) &body sub-states)
		  `(statecharts::%register-state statechart superstate-key ,name 
						 (let ((superstate-key (append superstate-key (list ,name))))
						   (%o ,name ,description statechart
						       superstate-key current-selector
						       (list ,@sub-states)))))
		(:-> (event initial final &key if)
		  `(let* ((initial-key (statecharts::%dereference-key superstate-key ,initial))
			  (final-key (statecharts::%dereference-key superstate-key ,final))
			  (trans-obj (statecharts::%t initial-key ,event final-key ,if)))
		     (statecharts::%register-event statechart ,event trans-obj)
		     (statecharts::%register-transition statechart superstate-key
							initial-key final-key trans-obj)))
		(:s (name &key (description "") entry exit)
		  `(statecharts::%register-state statechart superstate-key ,name 
						 (statecharts::%s ,name ,description ,entry ,exit
								  current-selector))))
       (progn ,@definitions))
     (defparameter ,name statechart)))


(defstatechart (test-states)
  (:h "D"
      (:c ("D")
	  (:d "A"
	      (:s "A" :entry (constantly nil))
	      (:s "C")
	      (:-> "hickup" "A" "C")))
      (:s "B")
      (:-> "fart" '("D" "A") "B")))








