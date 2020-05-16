

(in-package #:statecharts)


(defun %create-state-chart (name root description)
  (let* ((states (sc.chart::compute-substates root))
	 (transitions (sc.chart::compute-transitions root '() root))
	 (fsm-states (sc.fsm::create-states states))
	 (events  (remove-duplicates (mapcar #'sc.chart::event-name transitions)))
	 (default-state (first (remove-if-not #'sc.chart::is-default-state states))))
    (sc.fsm::set-transitions-for-states states fsm-states transitions)
    (make-instance 'statecharts::statechart
		   :name (string name)
		   :description description
		   :root root
		   :states states
		   :fsm-states fsm-states
		   :transitions transitions
		   :events events
		   :default-state default-state)))

(defun %check-defstatechart-arguments (name description definitions)
  (declare (ignore definitions))
  (cond
    ((not (symbolp name))
     (error 'sc.cond::invalid-chart-syntax
	    :message "Name needs to be a symbol."
	    :offending-code name))
    ((not (stringp description))
     (error 'sc.cond::invalid-chart-syntax
	    :message "Description needs to be a string."
	    :offending-code description))))


(defmacro defstatechart ((name &key (description ""))
			 &body definitions)
  (%check-defstatechart-arguments name description definitions)
  (sc.dsl::clear-id)
  `(defparameter ,name (%create-state-chart ',name
					    (let ((sc.dsl::*nth-sub-state* -1))
					      (progn ,@definitions))
					    ,description)))




