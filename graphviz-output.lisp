(in-package #:statecharts)


;; (defmethod  cl-dot:graph-object-node ((graph statechart) (object s))
;;   (make-instance 'cl-dot:node
;; 		 :attributes `(:label ,(with-output-to-string (stream)
;; 					 (print-s object stream))
;; 			       :shape :box)))


;; (defmethod cl-dot:graph-object-points-to ((graph statechart) (object s))
;;   (iter
;;     (for ev->final-state in (ev->state object))
;;     (collect (make-instance 'cl-dot:attributed
;; 			    :object (cdr ev->final-state)
;; 			    :attributes `(:weight 2
;; 					  :label ,(car ev->final-state))))))


(defgeneric style-options (s style)
  (:method ((s t) (style t)) '()))

(defgeneric nodes (state style)
  (:method ((s t) (style t))
    '()))


(defmethod nodes ((s state) style-options)
  `(:node ((:id ,(format nil "~D" (id s))) (:label ,(name s)) (:shape "record")
	   ,@(style-options s style-options))))


(defmethod nodes ((s cluster) style-options)
  `(:cluster ((:id ,(format nil "~D_cluster" (id s))) (:label ,(name s))
	      ,@(style-options s style-options))
     (:node ((:id ,(format nil "~D" (id s))) (:shape "point") (:style "invis")))
     ,@(iter
	 (for e in (elements s))
	 (for n = (nodes e style-options))
	 (if n (collect n)))))



(defgeneric edge (transition style root))



(defmethod edge ((transition tr) style root)
  (labels ((make-point (state-name from/to)
	     (let ((chart-element (find-dsl-object state-name root)))
	       (if (not chart-element)
		   (error "Couldn't find state with name: ~a" state-name))
	       (let ((id (format nil "~D" (id chart-element))))
		 (cond
		   ((eql :from from/to) 
		    `((:from ,id) ,@(if (typep chart-element 'cluster)
					`((:ltail ,(format nil "~D_cluster" id))))))
		   ((eql :to from/to) 
		    `((:to ,id) ,@(if (typep chart-element 'cluster)
				      `((:lhead ,(format nil "~D_cluster" id)))))))))))
    `(:edge (,@(make-point (initial-state-name transition) :from)
	     ,@(make-point (final-state-name transition) :to)
	     (:label ,(format nil "~a" (event-name transition)))
	     (:fontsize "10") (:fontname "Courier"))))))




(defun render-to-dot (statechart filename &key (output-format "png") (style-options t))
  (let+ (((&slots root transitions) statechart)
	 (graph `(:graph ()
			 ,(nodes root style-options)
			 ,@(iter
			     (for tr in transitions)
			     (collect (edge tr style-options root))))))
    (format t "~a" graph)
    (s-dot:render-s-dot filename output-format graph)))
