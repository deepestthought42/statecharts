(in-package #:statecharts)



;; let's start by reproducing Fig. 2 of





(defstatechart (test-states)
  (:s "A" :entry (constantly nil))
  (:s "C")
  (:-> "" "A" "C" ))




