; Homework 2 ;

;(setf *print-circle* nil)

; Question 1 

; BFS takes in a single argument FRINGE reprented as a list of search trees (list of lists) and returns 
; a list of leaf nodes in the order in which they were visited from left to right using a 
; breadth first search. If the argument FRINGE is a single element, it returns the element
; as the root of the search tree.
(defun BFS (FRINGE)
	(cond 	((not FRINGE) nil)
			((atom (first FRINGE)) (cons (first FRINGE) (BFS (rest FRINGE)))) ; case where first element visited is an atom
			(t (BFS (append (cdr FRINGE) (first FRINGE)))) ;case where first visited is still a list
		)
)
; Question 2

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (if (equal '(T T T T) S)
    	t
   		nil
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A) 
	(let ((currentstate
		(cond 	
			((equal A 'h) 
			(if (equal (first S) t)
					(append '(NIL) (rest S))
					(append '(T) (rest S))
					))
			((equal A 'b) 
				(if (equal (first S) (second S))
				(if (and (equal (first S) t) (equal (second S) t)) 
						(append '(NIL) '(NIL) (list(third S)) (list(fourth S)))
						(append '(T) '(T) (list(third S)) (list(fourth S)))
						)
					))
			((equal A 'd) 
				(if (equal (first S) (third S))
					(if	(and (equal (first S) t) (equal (third S) t)) 
						(append '(NIL) (list(second S)) '(NIL) (list(fourth S)))
						(append '(T) (list(second S)) '(T) (list(fourth S)))
						)
					nil
					))
			((equal A 'p) 
				(if (equal (first S) (fourth S))
					(if (and (equal (first S) t) (equal (fourth S) t)) 
						(append '(NIL) (list(second S)) (list(third S)) '(NIL))
						(append '(T) (list(second S)) (list(third S)) '(T))
						)
					))
			)
			))
			(cond
				((not currentstate) nil)
				((or (equal (first currentstate) (second currentstate)) 
				(and (equal (first currentstate) (third currentstate)) (equal (first currentstate) (fourth currentstate))))
				(list currentstate))

		)
	)
)

; SUCC-FN returns all of the possible legal successor states to the current state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators to the current state.

(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond 
    	((not STATES) nil)
    	(t (if (equal S (car STATES))
    			t
    			(ON-PATH S (cdr STATES))
    		)
    	)
    )
)


; MULT-DFS is a helper function for DFS. It takes two arguments: a list of states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the complete path from the initial state to the goal state. Otherwise, it returns NIL.
(defun MULT-DFS (STATES PATH)
    (if (not STATES) 
    	nil
		(let ((foundpath (DFS (first STATES) PATH)))
    		(if (not foundpath)
    			(MULT-DFS (rest STATES) PATH)
    			foundpath
    		)
    	)
    )
)

(defun DFS (S PATH)
	(if (equal (FINAL-STATE S) t) ;if we have reached goal state, return the path
		(append PATH (list S))
		(if (equal (ON-PATH S PATH) t) ;if we are testing a state we have already seen, return nil to go back to MULT-DFS and try next states
			nil
			(let ((currentpath (append PATH (list S))))
				(MULT-DFS (SUCC-FN S) currentpath)
			) ;otherwise append this state and go one more layer of recursion deep with the next functions
		)
    )
)

;(print (DFS '(NIL NIL NIL NIL) NIL))
