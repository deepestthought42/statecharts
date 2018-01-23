(in-package :statecharts)



(parachute:define-test chart-definition
  (parachute:finish
   (defstatechart (test-chart-1)
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
   (parachute:finish
    (defstatechart (test-chart-2)
      (c "H" (d "G")
	(o "G" (d "X")
	  (c "X" (d "A")
	    (s "A")
	    (s "B"))
	  (c "Y" (d "B")
	    (s "A")
	    (s "B")
	    (-> "alpha" "B" "A")
	    (-> "beta" "A" "B")
	    )
	  (c "Z" (d "B")
	    (s "A")
	    (s "B")
	    (s "C")
	    (-> "alpha" "A" "B")
	    (-> "beta" "B" "C")
	    (-> "gamma" "C" "A")
	    (-> "epsilon" "C" '(/ "H" ("G" ("X" "A"))))))
	(-> "alpha" "Z" '("G" (:and
			       ("Y" "A")
			       ("X" "B")))))))))


(parachute:define-test make-keys
  :depends-on (chart-definition)

  (parachute:finish (make-key '("test") (root test-chart-1)))
  (parachute:finish (make-key '("test" "X" "B") (root test-chart-1)))

  (parachute:fail (make-key '("H" "G") (root test-chart-1)))
  (parachute:finish (make-key '("H" "G") (root test-chart-2)))
  (parachute:finish (make-key '("H" "G" ("X" "A")) (root test-chart-2)))
  (parachute:fail (make-key '("H" "G" ("X" "A") ("X" "B")) (root test-chart-2))))


(parachute:test 'make-keys)
