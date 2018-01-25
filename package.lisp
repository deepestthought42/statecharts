;;;; package.lisp

(defpackage #:statecharts
  (:use #:cl #:iterate #:let-plus)
  (:nicknames #:sc)
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
   #:invalid-chart-syntax
   #:->
   #:d
   #:h
   #:c
   #:o
   #:defstatechart
   #:s
   #:invalid-transition
   #:state-a
   #:state-b
   #:reason
   #:couldnt-join-state-names))

