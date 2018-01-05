;;;; package.lisp

(defpackage #:statecharts
  (:use #:cl #:iterate #:let-plus)
  (:export
   #:statechart-element
   #:event
   #:*no-event*
   #:*unknown*
   #:environment
   #:transition
   #:action
   #:state
   #:history
   #:cluster
   #:orthogonal
   #:state-exists
   #:transition-exists
   #:invalid-state-descriptor
   #:invalid-chart-syntax))

