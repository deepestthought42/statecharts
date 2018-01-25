(in-package #:statecharts)


(defmethod  cl-dot:graph-object-node ((graph statechart) (object s))
  (make-instance 'cl-dot:node
		 :attributes `(:label ,(with-output-to-string (stream)
					 (print-s object stream))
			       :shape :box)))


(defmethod cl-dot:graph-object-points-to ((graph statechart) (object s))
  (iter
    (for ev->final-state in (ev->state object))
    (collect (make-instance 'cl-dot:attributed
			    :object (cdr ev->final-state)
			    :attributes `(:weight 2
					  :label ,(car ev->final-state))))))



