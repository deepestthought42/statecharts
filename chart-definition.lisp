(in-package #:statecharts)

(defun %register-event (statechart event-name transition-object)
  (let+ (((&slots events) statechart)
	 (event (alexandria:if-let (existing (gethash event-name events))
		  existing
		  (setf (gethash event-name events)
			(make-instance 'event :name event-name)))))
    (push transition-object (registered-transitions event))))

(defun %t (initial-state event final-state if)
  (make-instance 'transition
		 :name (concatenate 'string
				    initial-state
				    "->" (if if if "")
				    final-state)
		 :event event
		 :initial-state initial-state
		 :final-state final-state
		 :guard (if if if (constantly t))))


(defun %s (name description entry exit)
  (make-instance 'state :name name :description description
			:on-entry (if entry entry (constantly t))
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
	    (mapcar #'valid-descriptor superstate))
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



(defun %register-transition (statechart superstate initial final transition-object)
  (let+ (((&slots transitions) statechart)
	 (initial-key (%dereference-key superstate initial))
	 (final-key (%dereference-key superstate final))
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
  (alexandria:with-gensyms (statechart superstate)
    `(let* ((,statechart (make-instance 'statechart :name ,(string name)
						    :description ,description))
	    (,superstate ()))
       (labels ((:-> (event initial-state final-state &key if)
		  (let ((trans-obj (%t initial-state event final-state if)))
		    (%register-event ,statechart event trans-obj)
		    (%register-transition ,statechart ,superstate
					  initial-state final-state trans-obj)))
		(:s (name &key (description "") entry exit)
		  (%register-state ,statechart ,superstate name
				   (%s name description entry exit))))
	 (progn ,@definitions)
	 (defparameter ,name ,statechart)))))


(defstatechart (test-states)
  (:s "A" :entry (constantly nil))
  (:s "C")
  (:-> "" "A" "C" ))
