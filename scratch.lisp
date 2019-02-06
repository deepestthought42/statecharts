(in-package #:statecharts)



;; let's start by reproducing Fig. 2 of

(defstatechart (test-states)
  (c "test" (d "X" :entry (sc:act () (format t "Yes")))
    (s "A")
    (c "X" (d "A")
      (s "A")
      (s "B"))
    (c "Y" (d "A")
      (s "A")
      (s "B"))
    (c "Z" (d "A")
      (s "A")
      (s "B"))
    (-> "hickup" "A" "Y")
    (-> "poo" "X" "Z")))


(defstatechart (test-1)
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
	(s "A"
	   :entry
	   (sc:act "test action" (env)
	     (declare (ignore env))
	     (format t "Action!")))
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
       (dgraph (cl-dot:generate-graph-from-roots test-states (states test-1)
						 '(:rankdir "LR"))))
  (cl-dot:dot-graph dgraph "/home/renee/tmp/test-lr.png" :format :png))


(unchain-all-states (states test-1))

(defparameter *test-fsm* (create-fsm-runtime test-1 :debug t))

(signal-event *test-fsm* 'sc::|alpha|)



(create-fsm-states (states test-states))




(defstatechart (test-states)
  (c "G" (d "Z")
    (c "Z" (d "A")
      (s "A")
      (s "B"))
    (o "X" ()
      (c "A" (sc:d "1")
	(s "1")
	(s "2")
	(-> "A to-2" "1" "2")
	(-> "A to-1" "2" "1"))
      (c "B" (sc:d "1")
	(s "1")
	(s "2")
	(-> "B to-2" "1" "2")
	(-> "B to-1" "2" "1")))
    (-> "init" "Z" "X")))

(create-fsm-runtime test-states)

(defstatechart (test-states-2)
  (c "G" (d "Z" :entry (act () (format t "hello")))
    (s "Z" :entry (act ()))
    (o "X" ()
      (c "A" (d "1") 
	(s "1")
	(s "2")
	(-> "A to-2" "1" "2")
	(-> "A to-1" "2" "1")
	(-> "switch" "1" "2")
	(-> "switch" "2" "1"))
      (c "B" (d "1")
	(s "1")
	(s "2")
	(-> "switch" "1" "2")
	(-> "switch" "2" "1")
	(-> "B to-2" "1" "2")
	(-> "B to-1" "2" "1")))
    (-> "init" "Z" "X")))

(create-fsm-runtime test-states-2)

(difference-state-names (make-state-name '("G" "Z" "A") (root test-states))
			(make-state-name '("G" "Z" "B") (root test-states)))







(defstatechart (sc/test)
  (sc:c "test" (sc:d "a")
    (sc:s "a" :reentry (act (e) (format t "entry~%")))
    (sc:s "b")
    (sc:-> "ev" "a" "b")
    (sc:-> "ev" "b" "a")))


(let ((env (make-instance 'environment
			  :fsm (create-fsm-runtime sc/test :debug t))))
  (signal-event env '|ev|)
  (signal-event env '|ev|)
  (signal-event env '|ev|))
