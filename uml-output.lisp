(defpackage #:statecharts.uml
  (:use #:cl #:iterate #:let-plus #:trivia #:sc.cond)
  (:nicknames #:sc.uml))

(in-package #:statecharts)


(defparameter *indentation-step* 2)



(macrolet ((f (str &rest args)
	     `(progn
		(format stream "~v@{~A~:*~}" (+ indent *indentation-step*) " ")
		(format stream ,str ,@args))))
  (defgeneric nodes (s stream &key indent)
    (:method ((s t) stream &key indent)
      (declare (ignore s stream indent))))

  (defmethod nodes ((s sc.dsl::state) stream &key (indent 0))
    (f "state \"~a\" as s~D~%" (sc.dsl::name s) (sc.dsl::id s)))
  
  (defmethod nodes ((s sc.dsl::cluster) stream &key (indent 0))
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
	(nodes e stream :indent (+ indent *indentation-step*)))
      (f "}~%")))
  
  (defmethod nodes ((s sc.dsl::orthogonal) stream &key (indent 0))
    (f "state \"~a\" as s~D {~%" (sc.dsl::name s) (sc.dsl::id s))
    (iter
      (with els = (sc.dsl::elements s))
      (for e in (subseq els 0 (1- (length els))))
      (nodes e stream :indent (+ indent *indentation-step*) )
      (f "||~%")
      (finally
       (nodes (first (last els)) stream
	      :indent (+ indent *indentation-step*) )))
    (f "}~%"))

  
  (defgeneric edge (transition root stream &key indent))

  (defmethod edge ((transition sc.chart::transition) root stream &key (indent 0))
    (labels ((gid (state-name)
	       (let ((chart-element (sc.key::find-dsl-object state-name root)))
		 (if (not chart-element)
		     (error "Couldn't find state with name: ~a" state-name))
		 (format nil "s~D" (sc.dsl::id chart-element)))))
      ;; fixme: this doesn't work anymore
      (f "~a --> ~a : ~a~%"
	 (gid (sc.chart::initial-state-name transition))
	 (gid (sc.chart::final-state-name transition)) ;
	 (sc.chart::event-name transition)))))


(defgeneric render-to-uml (root filename &key))

(defmethod render-to-uml ((root sc.dsl::state) filename
			  &key (if-exists :supersede))
  (with-open-file (stream filename
			  :direction :output :if-exists if-exists)
    (format stream "@startuml~%")
    (format stream "skinparam classFontSize 9~%skinparam classFontName PragmataPro~%")
    (format stream "hide empty description~%")
    (nodes root stream)
    (iter
      (for tr in (sc.chart::compute-transitions root '() root))
      (edge tr root stream))
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
