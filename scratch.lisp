(in-package #:statecharts)



;; let's start by reproducing Fig. 2 of

(defstatechart (test-states)
  (c "test" (d "X")
    (c "X" (d "A")
      (s "A")
      (s "B"))
    (c "Y" (d "A")
      (s "A")
      (s "B"))
    (c "Z" (d "A")
      (s "A")
      (s "B"))
    (-> "hickup" '("X" "A") '("Y"))
    (-> "poo" "X" "Z")))


(defstatechart (test-1)
  (c "H" (d "G")
    (o "G" (d "X")
      (c "X" (d "A")
	(s "A")
	(s "B"))
      (c "Y" (d "B")
	(s "A")
	(s "B")
;;	(-> "alpha" "B" "A")
;;	(-> "beta" "A" "B")
	)
      (c "Z" (d "B")
	(s "A")
	(s "B")
	(s "C")
;;	(-> "alpha" "A" "B")
;;	(-> "beta" "B" "C")
;;	(-> "gamma" "C" "A")
	(-> "epsilon" "C" '(/ "H" ("G" ("X" "A"))))))
    ;; (-> "alpha" "Z" '("G" (:and
    ;; 			   ("Y" "A")
    ;; 			   ("X" "B"))))
    ))




(let ((state-chart test-1))
  (find-final-states-for-transitions (states state-chart)
				     (gather-events-from-transitions
				      (transitions state-chart))))



(find-final-states-for-transitions (states test-1)
				   (transitions test-1))








(remove-if-not #'is-default-state (states test-1))




