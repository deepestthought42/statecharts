(in-package #:statecharts)



;; let's start by reproducing Fig. 2 of

(defstatechart (test-states)
  (c "D" (h "A")
    (s "A" :entry (constantly nil))
    (s "C")
    (-> "hickup" "A" "C"))
  (s "B")
  (-> "fart" '("D" "A") "B"))








