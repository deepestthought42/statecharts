(in-package #:statecharts)

(declaim (optimize (debug 3) (speed 0) (space 0)))

;;; definitions of objects to be used in when composing a FSM out of
;;; statecharts


(defun state-p (obj)
  (typep obj 'state))


(defclass s ()
  ((name :initarg :name :accessor name :initform (error "Must initialize name."))
   (defining-state :initarg :defining-state :accessor defining-state 
		   :initform (error "Must initialize defining-state."))
   (on-entry :accessor on-entry :initarg :on-entry :initform '())
   (on-exit :accessor on-exit :initarg :on-exit :initform '())))


(defclass s-xor (s)
  ((sub-state :initarg :sub-state :accessor sub-state 
	      :initform (error "Must initialize sub-state."))
   (default-state :initarg :default-state :accessor default-state 
		  :initform (error "Must initialize default-state."))))

(defclass s-and (s)
  ((sub-states :initarg :sub-states :accessor sub-states 
	       :initform (error "Must initialize sub-states."))))


(defclass tr ()
  ((initial-state-name :initarg :initial-state-name :accessor initial-state-name 
		       :initform (error "Must initialize initial-state-name."))
   (final-state-name :initarg :final-state-name :accessor final-state-name 
		     :initform (error "Must initialize final-state-name."))
   (event-name :initarg :event-name :accessor event-name 
	      :initform (error "Must initialize event-state-name."))
   (guard :initarg :guard :accessor guard 
	  :initform (error "Must initialize guard."))))

;;; printing


(defmethod print-object ((obj tr) stream)
  (print-unreadable-object (obj stream)
    (format stream "~a --|~a|--> ~a"
	    (initial-state-name obj)
	    (event-name obj)
	    (final-state-name obj))))


(defmethod print-object ((obj s) stream)
  (print-unreadable-object (obj stream)
    (print-s obj stream)))

(defmethod print-s ((obj s) stream)
  (format stream "~a" (name obj)))

(defmethod print-s ((obj s-xor) stream)
  (format stream "(~a " (name obj))
  (print-s (sub-state obj) stream)
  (format stream ")"))


(defmethod print-s ((obj s-and) stream)
  (format stream "(~a " (name obj))
  (let ((sub-states (sub-states obj)))
    (when sub-states
      (print-s (car sub-states) stream)
      (map nil #'(lambda (s)
	   (format stream "∧")
		   (print-s s stream))
	   (cdr sub-states))))
  (format stream ")"))


;;; state to name comparison

;;; 

(defgeneric state=state-name (state state-name))

(defmethod  state=state-name ((state t)
			      (state-name t))
  nil)

(defmethod  state=state-name ((state s)
			      (state-name state-name))
  (string= (name state-name)
	   (name state)))

(defmethod  state=state-name ((state s-xor)
			      (state-name or-state-name))
  (string= (name state-name)
	   (name state)))

(defmethod  state=state-name ((state s-and)
			      (state-name and-state-name))
  (string= (name state-name)
	   (name state)))



(defgeneric state-described-by-name (s state-name))

(defmethod state-described-by-name ((s t) state-name) nil)

(defmethod state-described-by-name ((s s) state-name)
  (state=state-name s state-name))

(defmethod state-described-by-name ((s s-xor) state-name)
  (cond
    ;; name and state-name have to match as well as the type of name
    ((not (state=state-name s state-name)) nil)
    ;; the name matches but doesn't specify any sub-states -> match
    ((not (sub-state state-name)) t)
    ;; the name still matches and specifies sub-states -> test substate
    (t (state-described-by-name (sub-state s) (sub-state state-name)))))

(defmethod state-described-by-name ((s s-and) state-name)
  (labels ((is-sub-state (k)
	     (iter
	       (for sub-s in (sub-states s))
	       (if (state-described-by-name sub-s k)
		   (return t)))))
    (cond
      ;; name and state-name have to match as well as the type of state-name
      ((not (state=state-name s state-name)) nil)
      ;; the state-name doesn't specify any substates, so all good
      ((not (sub-states state-name)) t)
      ;; name matches and we have substates, so compare them one be one
      (t (reduce #'(lambda (a b) (and a b))
		 (sub-states state-name)
		 :key #'is-sub-state)))))



;;; default states

(defmethod is-default-state ((s t)) nil)


(defmethod is-default-state ((s s)) t)


(defmethod is-default-state ((s s-xor))
  (let+ (((&slots default-state sub-state) s))
    (cond
      ((string= (name sub-state) default-state)
       (is-default-state sub-state))
      ;; no default state
      (t nil))))


(defmethod is-default-state ((s s-and))
  (reduce #'(lambda (a b)
	      (and a b))
	  (sub-states s)
	  :key #'is-default-state))



;;; these methods assume states S that correspond to the name given

(defgeneric %is-partial-default-state (s state-name))

(defmethod %is-partial-default-state ((s s) (state-name state-name)) t)

(defmethod %is-partial-default-state ((s s-xor) (state-name or-state-name))
  (if (sub-state state-name)
      (%is-partial-default-state (sub-state s) (sub-state state-name))
      (is-default-state s)))


(defmethod %is-partial-default-state ((s s-and) (state-name and-state-name))
  (labels ((find-state-name (sub-s state-names)
	     (iter
	       (for state-name in state-names)
	       (if (state-described-by-name sub-s state-name)
		   (return state-name)))))
    (iter
      (for sub-s in (sub-states s))
      (for s-name = (find-state-name sub-s (sub-states state-name)))
      (cond
	((and s-name (not (%is-partial-default-state sub-s s-name))) (return nil))
	((and (not s-name) (not (is-default-state sub-s))) (return nil)))
      (finally (return t)))))

(defun get-partial-default-state (lst-of-states state-name)
  (let+ ((described-states
	  (remove-if-not #'(lambda (s)
			     (state-described-by-name s state-name))
			 lst-of-states)))
    (remove-if-not #'(lambda (s) (%is-partial-default-state s state-name)) described-states)))

