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
	(-> "alpha" "B" "A")
	(-> "beta" "A" "B"))
      (c "Z" (d "B")
	(s "A")
	(s "B")
	(s "C")
;;	(-> "alpha" "A" "B")
	(-> "beta" "B" "C")
	(-> "gamma" "C" "A")
	(-> "epsilon"
	    "C"
	    '(:/ "H" "G" ("X" "A")))))
    (-> "alpha"
	'("G" "Z")
	'("G" ("Y" "A")
	      ("X" "B")))))


(join-state-names (make-state-name '("H" "G" ("Z" "C")) (root test-1))
		  (make-state-name '("H" "G" ("Z" "A")) (root test-1)))



(state-name= (make-state-name '("H" "G" ("Z" "C")) (root test-1))
	     (make-state-name '("H" "G" ("Z" "C")) (root test-1)))



(find-final-states-for-transitions (states test-1)
				   (transitions test-1))

(let ((name ))
  (remove-if-not #'is-default-state
		 (remove-if-not #'(lambda (s) (state-described-by-name s name))
				(remove-if-not #'(lambda (s) (state-described-by-name s name))
					       (states test-1)))))


(let ((name (make-state-name '("H" "G" ("X" "A") ("Y" "B")) (root test-1))))
  (get-partial-default-state (states test-1) name))





(described-by-final-keys? (states test-1) '(("H" ("G" ("Y" "B"))) ("H" ("G" ("Z" "C")))))


(remove-if-not #'is-default-state (states test-1))



(find-final-states-for-transitions (states test-states) (transitions test-states))



(let* ((cl-dot:*dot-path* "/usr/bin/dot")
       (data '(a b c #1=(b z) c d #1#))
       (dgraph (cl-dot:generate-graph-from-roots test-states (states test-states)
						 '(:rankdir "LR"))))
  (cl-dot:dot-graph dgraph "/home/renee/tmp/test-lr.png" :format :png))


