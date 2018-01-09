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
    (-> "hickup" "A" "C")
    (-> "poo" '("D" "A") '("M" "A"))))


(defstatechart (test-1)
  (c "H" (d "A")
    (s "Z")
    (o "G" (d "X")
      (c "X" (d "A")
	(s "A")
	(s "B"))
      (c "Y" (d "A")
	(s "A")
	(s "B"))
      (c "Z" (d "A")
	(s "A")
	(s "B")
	(s "C")))))

(get-substates (root test-1))

























(combine-elements '((("a" "b") ("a" "c")) (("d" "e") ("d" "f"))))



























