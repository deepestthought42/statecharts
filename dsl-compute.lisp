(in-package #:statecharts.dsl)

(defclass context ()
  ((current-bit :accessor current-bit :initarg :current-bit
		:initform 1)))

(defun get-next-state-bit (context is-in-cluster)
  (if is-in-cluster
      (prog1
	  (current-bit context)
	(setf (current-bit context)
	      (ash (current-bit context) 1)))
      0))



(defgeneric set-state-bits (state context &optional is-in-cluster)
  (:documentation "Travels through the tree for the CHART with node
  STATE and sets the state and substate bits for each state node.

STATE --- an object of (super)type STATE.
CONTEXT --- an object of tyep CONTEXT.

Returns => an integer describing the sub-states."))



(defmethod set-state-bits ((element statechart-element) context
			   &optional is-in-cluster)
  (declare (ignore element context is-in-cluster))
  0)

(defmethod set-state-bits ((state state) context
			   &optional is-in-cluster)
  (setf (state-bit state) (get-next-state-bit context is-in-cluster)))


(defun %set-state-bits (state context is-in-cluster called-from-cluster)
  (let+ ((sub-states-bits (iter
			    (for sub in (elements state))
			    (sum (set-state-bits sub context called-from-cluster))))
	 (state-bit (get-next-state-bit context is-in-cluster)))
    (setf (state-bit state) state-bit
	  (sub-states-bits state) sub-states-bits)
    (+ state-bit sub-states-bits)))


(defmethod set-state-bits ((state cluster) context &optional is-in-cluster)
  (%set-state-bits state context is-in-cluster t))

(defmethod set-state-bits ((state orthogonal) context &optional is-in-cluster)
  (%set-state-bits state context is-in-cluster nil))

