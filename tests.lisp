(in-package :statecharts)



(parachute:define-test chart-definition
  (parachute:finish
   (defstatechart (test-chart-1)
     (c "test" (d "X")
       (c "X" (d "A")
	 (s "A")
	 (s "B"))
       (c "Y" (d "A")
	 (s "A" :entry (act "test"))
	 (s "B"))
       (c "Z" (d "A")
	 (s "A")
	 (s "B"))
       (-> "hickup" '("X" "A") '("Y"))
       (-> "poo" "X" "Z")))
   (parachute:finish
    (defstatechart (test-chart-2)
      (c "H" (d "G")
	(o "G" ()
	  (c "X" (d "A")
	    (s "A")
	    (s "B"))
	  (c "Y" (d "B")
	    (s "A")
	    (s "B")
	    (-> "alpha" "B" "A")
	    (-> "beta" "A" "B"))
	  (c "Z" (d "B")
	    (s "A")
	    (s "B")
	    (s "C")
	    (-> "alpha" "A" "B")
	    (-> "beta" "B" "C")
	    (-> "gamma" "C" "A")
	    (-> "epsilon" "C"
		'(:/ "H" "G" "X" "A"))))
	(-> "alpha"
	    '("G" "Z")
	    '("G" 
	      ("Y" "A")
	      ("X" "B"))))))
   (parachute:fail
    (defstatechart (test-chart-wrong-default)
      (c "H" (d "G")
	(s "A")
	(s "B"))))))


(parachute:define-test make-state-name
  :depends-on (chart-definition)

  (parachute:finish (make-state-name '("test") (root test-chart-1)))
  (parachute:finish (make-state-name '("test" "X" "B") (root test-chart-1)))

  (parachute:fail (make-state-name '("H" "G") (root test-chart-1)))
  (parachute:finish (make-state-name '("H" "G") (root test-chart-2)))
  (parachute:finish (make-state-name '("H" "G" "X" "A") (root test-chart-2)))
  (parachute:fail (make-state-name '("H" "G" ("X" "A") ("X" "B")) (root test-chart-2))))




(labels ((%described-by-name (chart description)
	   (let ((name (make-state-name description (root chart))))
	     (remove-if-not #'(lambda (s)
				(state-described-by-name s name))
			    (states chart)))))
  (parachute:define-test state-described-by-name
    :depends-on (make-state-name)
    (parachute:is = 1 (length
		       (%described-by-name test-chart-2 '("H"
							  "G"
							  ("X" "A")
							  ("Y" "A")
							  ("Z" "A")))))
    (parachute:is = 3 (length
		       (%described-by-name test-chart-2 '("H"
							  "G"
							  ("X" "A")
							  ("Y" "A")))))
    (parachute:is = 12 (length
			(%described-by-name test-chart-2 '("H" "G"))))))

(labels ((%partial (chart description)
	   (get-partial-default-state
	    (states chart) (make-state-name description (root chart)))))
  (parachute:define-test partial-default-state
    :depends-on (state-described-by-name)
    (parachute:true (%partial test-chart-2 '("H" "G"
					     ("X" "A")
					     ("Y" "A")
					     ("Z" "A"))))
    (parachute:true (%partial test-chart-2 '("H" "G")))
    (parachute:true (%partial test-chart-2 '("H" "G" "X" "B")))))




(parachute:test 'partial-default-state)
