(defpackage #:statecharts.uml
  (:use #:cl #:iterate #:let-plus #:trivia #:sc.cond)
  (:nicknames #:sc.uml))

(in-package #:statecharts)


(defparameter *indentation-step* 2)

(defun make-fork-state-name (initial-state-name event-name)
  (format nil "fork_~a_~a" initial-state-name event-name))

(defun get-state-id (root state-name)
  (let ((chart-element (sc.key::find-dsl-object state-name root)))
    (if (not chart-element)
	(error "Couldn't find state with name: ~a" state-name))
    (format nil "s~D" (sc.dsl::id chart-element))))


(defun get-transitions-for-state (chart-element all-transitions root)
  (remove-if-not #'(lambda (tr)
		     (eql (sc.key::find-dsl-object
			   (sc.chart::initial-state-name tr)
			   root)
			  chart-element))
		 all-transitions))

(macrolet ((f (str &rest args)
	     `(progn
		(format stream "~v@{~A~:*~}"
			(+ indent *indentation-step*)
			" ")
		(format stream ,str ,@args))))
  
  (defun create-fork-states (stream transitions root &key (indent 0))
    (iter
      (for tr in transitions)
      (when (> (length (sc.chart::clauses tr)) 1)
	(f "state ~a <<fork>>~%"
	   (make-fork-state-name
	    (get-state-id root (sc.chart::initial-state-name tr))
	    (sc.chart::event-name tr))))))
  
  (defgeneric nodes (s stream transitions &key indent root)
    (:method ((s t) stream transitions &key indent root)
      (declare (ignore s stream transitions indent root))))

  (defmethod nodes ((s sc.dsl::state) stream transitions &key (indent 0) (root s))
    (f "state \"~a\" as s~D~%" (sc.dsl::name s) (sc.dsl::id s))
    (create-fork-states stream
			(get-transitions-for-state s transitions root)
			root :indent indent))
  
  (defmethod nodes ((s sc.dsl::cluster) stream transitions &key (indent 0) (root s))
    (labels ((find-default ()
	       (alexandria:if-let (def (find (sc.dsl::default-state s)
					     (sc.dsl::elements s)
					     :key #'sc.dsl::name))
		 (sc.dsl::id def)
		 (error "Couldn't find default state."))))
      (f "state \"~a\" as s~D {~%" (sc.dsl::name s) (sc.dsl::id s))
      (f "[*] --> s~D~%" (find-default))
      (iter
	(for e in (sc.dsl::elements s))
	(nodes e stream transitions :indent (+ indent *indentation-step*) :root root))
      (create-fork-states stream (get-transitions-for-state s transitions root)
			  root :indent indent)
      (f "}~%")))
  
  (defmethod nodes ((s sc.dsl::orthogonal) stream transitions &key (indent 0) (root s))
    (f "state \"~a\" as s~D {~%" (sc.dsl::name s) (sc.dsl::id s))
    (create-fork-states stream (get-transitions-for-state s transitions root)
			root :indent indent)
    (iter
      (with els = (sc.dsl::elements s))
      (for e in (subseq els 0 (1- (length els))))
      (nodes e stream transitions :indent (+ indent *indentation-step*) :root root)
      (f "||~%")
      (finally
	  (nodes (first (last els)) stream transitions
		 :indent (+ indent *indentation-step*))))
    (f "}~%"))

  
  (defgeneric edge (transition root stream &key indent))

  (defmethod edge ((transition sc.chart::transition) root stream &key (indent 0))
    (labels ((gid (state-name) (get-state-id root state-name)))
      (iter
	(with clauses = (sc.chart::clauses transition))
	(with start-state =
	      (cond
		((= (length clauses) 1)
		 (gid (sc.chart::initial-state-name transition)))
		((>= (length clauses) 1)
		 (let* ((event-name (sc.chart::event-name transition))
			(initial-state (gid (sc.chart::initial-state-name transition)))
			(fork_state (make-fork-state-name initial-state event-name)))
		   (f "~a --> ~a : ~a~%" initial-state fork_state event-name)
		   fork_state))
		(t (error "Something is wrong here ... and it shouldn't be."))))
	(for clause in clauses)
	(typecase clause
	  (sc.chart::guard-clause
	   (f "~a --> ~a : ~a~%"
	      start-state
	      (gid (sc.dsl::final-state clause))
	      (sc.dsl::code clause)))
	  (sc.chart::otherwise-clause
	   (f "~a --> ~a : OTHERWISE~%"
	      start-state
	      (gid (sc.dsl::final-state clause))))
	  (sc.chart::transition-clause
	   (f "~a --> ~a : ~a~%"
	      start-state
	      (gid (sc.dsl::final-state clause)) ;
	      (sc.chart::event-name transition))))))))


(defgeneric render-to-uml (root filename &key))

(defmethod render-to-uml ((root sc.dsl::state) filename
			  &key (if-exists :supersede))
  (with-open-file (stream filename
			  :direction :output :if-exists if-exists)
    (format stream "@startuml~%")
    (format stream "skinparam classFontSize 9~%")
    (format stream "hide empty description~%")
    (let ((transitions (sc.chart::compute-transitions root '() root)))
      (nodes root stream transitions)
      (iter
	(for tr in transitions)
	(edge tr root stream)))
    (format stream "@enduml~%")))



(defmethod render-to-uml ((statechart statechart) filename
			  &key (if-exists :supersede))
  (let+ (((&slots root) statechart))
    (render-to-uml root filename
		   :if-exists if-exists)))


(defparameter *plant-uml-exe* "plantuml")


(defgeneric render (obj filename &key))

(defmethod render ((obj t) filename &key (output-type "png"))
  (let ((uml-filename (make-pathname :directory (pathname-directory filename)
				      :name (pathname-name filename) :type "uml")))
    (render-to-uml obj uml-filename)
    (uiop:run-program (format nil "~a -t~a ~a" *plant-uml-exe* output-type uml-filename)
    		      :ignore-error-status nil)))
