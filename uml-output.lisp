(in-package #:statecharts)



(defparameter *indentation-step* 2)

(macrolet ((f (str &rest args)
	     `(progn
		(format stream "~v@{~A~:*~}" (+ indent *indentation-step*) " ")
		(format stream ,str ,@args))))
  (defgeneric nodes (s stream &key indent)
    (:method ((s t) stream &key indent)))

  (defmethod nodes ((s state) stream &key (indent 0))
    (f "state \"~a\" as s~D~%" (name s) (id s)))
  
  (defmethod nodes ((s cluster) stream &key (indent 0))
    (labels ((find-default ()
	       (alexandria:if-let (def (find (default-state s)
					     (elements s)
					     :key #'name))
		 (id def)
		 (error "Couldn't find default state."))))
      (f "state \"~a\" as s~D {~%" (name s) (id s))
      (f "[*] --> s~D~%" (find-default))
      (iter
	(for e in (elements s))
	(nodes e stream :indent (+ indent *indentation-step*)))
      (f "}~%")))
  
  (defmethod nodes ((s orthogonal) stream &key (indent 0))
    (f "state \"~a\" as s~D {~%" (name s) (id s))
    (iter
      (with els = (elements s))
      (for e in (subseq els 0 (1- (length els))))
      (nodes e stream :indent (+ indent *indentation-step*) )
      (f "||~%")
      (finally
       (nodes (first (last els)) stream
	      :indent (+ indent *indentation-step*) )))
    (f "}~%"))

  
  (defgeneric edge (transition root stream &key indent))

  (defmethod edge ((transition tr) root stream &key (indent 0))
    (labels ((gid (state-name)
	       (let ((chart-element (find-dsl-object state-name root)))
		 (if (not chart-element)
		     (error "Couldn't find state with name: ~a" state-name))
		 (format nil "s~D" (id chart-element)))))
      (f "~a --> ~a : ~a~%"
	 (gid (initial-state-name transition))
	 (gid (final-state-name transition))
	 (event-name transition)))))


(defgeneric render-to-uml (root filename &key))

(defmethod render-to-uml ((root state) filename
			  &key (if-exists :supersede))
  (with-open-file (stream filename
			  :direction :output :if-exists if-exists)
    (format stream "@startuml~%")
    (nodes root stream)
    (iter
      (for tr in (compute-transitions root '() root))
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
