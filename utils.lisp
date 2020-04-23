(defpackage #:statecharts.utils
  (:use #:cl #:iterate #:let-plus #:trivia)
  (:nicknames #:sc.utils))


(in-package #:sc.utils)


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
      `(defmethod ,method-name
	   ((,object ,type)
	    &key ,@(mapcar #'(lambda (s)
			       `(,(slot-name s) (slot-value ,object ' ,(slot-name s))))
			   slots-to-maybe-copy))
	 (make-instance ',type
			,@(alexandria:mappend #'(lambda (s) (list (init-arg s) (slot-name s)))
					      slots-to-maybe-copy))))))






(defgeneric %print-object (object stream))

(defun object-str (obj)
  (with-output-to-string (stream)
    (%print-object obj stream)))

(defun combine-sets (sets)
  (cond
    ((= (length sets) 1)
     (iter
       (for el in (car sets))
       (collect (list el))))
    ((> (length sets) 1)
     (iter outer
       (with set = (car sets))
       (with rest = (combine-sets (cdr sets)))
       (for el in set)
       (iter
	 (for r in rest)
	 (in outer (collect (append (list el)
				    (copy-seq r)))))))))




(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
	     (cond ((> int 0)
		    (multiple-value-bind (i r) (truncate int 2)
		      (integer->bit-list i (push r accum))))
		   ((null accum) (push 0 accum))
		   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))
