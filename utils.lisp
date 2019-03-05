(in-package #:statecharts)



(defmacro define-copy-object-method ((type &optional (method-name 'copy-object))
				     &body slots-to-maybe-copy)
  (labels ((slot-name (s)
	     (match s
	       ((list slot-name _) slot-name)
	       ((type symbol) s)
	       (otherwise (error "Can't parse: ~a" s))))
	   (init-arg (s)
	     (match s
	       ((list _ init-arg)  init-arg)
	       ((type symbol) (alexandria:make-keyword s))
	       (otherwise (error "Can't parse: ~a" s)))))
    (alexandria:with-gensyms (object)
      `(defmethod ,method-name ((,object ,type)
				&key ,@(mapcar #'(lambda (s) `(,(slot-name s) (slot-value ,object ',(slot-name s))))
					slots-to-maybe-copy))
	 (make-instance ',type ,@(alexandria:mappend #'(lambda (s) (list (init-arg s) (slot-name s)))
						     slots-to-maybe-copy))))))






(defgeneric %print-object (object stream))
