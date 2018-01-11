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

(let ((key '("H" "G")))
  (remove-if-not #'(lambda (s)
		     (is-default-state s :of-state-key key))
		 (states test-1)))
 ; => (#<(H (G (X A)∧(Y B)∧(Z B)))>)


(let ((key '("test")))
  (remove-if-not #'(lambda (s)
		     (is-default-state s :of-state-key key))
		 (states test-states)))


(compute-transitions (root test-1) '())









