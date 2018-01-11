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
    (s "Z")
    (o "G" (d "X")
      (c "X" (d "A")
	(s "A")
	(s "B"))
      (c "Y" (d "B")
	(s "A")
	(s "B"))
      (c "Z" (d "B")
	(s "A")
	(s "B")
	(s "C")))
    (-> "a" "Z" '("G" (:and
		       ("Y" "A")
		       ("X" "B"))))))

(compute-substates (root test-1))


(get-partial-default-state (states test-1)
			   '("H" ("G" ("Z" "C"))))










(remove-if-not #'is-default-state (states test-1))



(cadr '("G" "H"))
