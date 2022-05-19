;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; You can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Beginning of actual Sokoban code
;

; Define global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow) returns a list indicating the position of the keeper (c r).
; Assumes that the keeper is in row >= firstRow. The top row is the zeroth row. The first (left) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
     (t (let ((x (getKeeperColumn (car s) 0)))
        (if x
		 ;keeper is in this row
          (list x row)
		 ;otherwise move on
          (getKeeperPosition (cdr s) (+ row 1))
          );end if
        );end let
     );end t
    );end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed. For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 


; This function to return true (t) if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; goal-test takes in a state of the Sobokan game and returns true (t) if it is a goal test.
; A box is represented with the value 2, therefore the function searches for the presence of the value
; 2 in state s and only returns true if 2 is not found
;
(defun goal-test (s)
  (if (atom s) 
  		(not (equal s 2))
  		(and (goal-test (car s))
  			(goal-test (cdr s))
  			)
  		)
  );end defun


; This function to return the list of sucessor states of s.
; This is the top-level next-states (successor) function.
; 'result' set to be the set of states after moving the keeper in each of the 4 directions:
; 
; next-states takes in a state s and returns a list of all the possible states that can be
; reached from state s. This function uses several helper functions including getKeeperPosition
; to identify the position of the keepr in state s as well as try-move, defined below, and 
; cleanUpList to remove any NIL elements in the result. The try-move function is called on state
; s using all four possible directions where each direction is represented as described in the
; inline comments for the function.

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos)) ; x represents the column
	 (y (cadr pos)) ; y represents the row
	 ; x and y are now the coordinate of the keeper in s.
	 ; (result (list (try-move s UP) (try-move s LEFT) (try-move s DOWN) (try-move s RIGHT)))
	 ; where UP = (y-1) x, LEFT = y (x-1), DOWN = (y+1) x, RIGHT = y (x+1)
	 (result (list (try-move s x y x (- y 1) 1) (try-move s x y (- x 1) y 4)
	 				(try-move s x y x (+ y 1) 3) (try-move s x y (+ x 1) y 2)))
	 )
    (cleanUpList result);end
   );end let
  );

; get-square is a helper function for next-states that takes in a state s, a row number r and
; a column number c and returns the current value of the square. If the coordinates (r,c) are
; out of bounds of the game, the function returns the value representation of a wall, 1.

(defun get-square (s r c)
	(if (or (> r (- (length s) 1)) (> c (- (length (car s)) 1)) (< c 0) (< r 0))
		1
	(nth c (nth r s))
	)
)

; findInRow is a helper function for set-square which is in turn a helper function for next-states.
; It takes in a row (list) of s, a column number c, and a square content v and
; returns the row with value in position c changed to v.

(defun findInRow (row c v)
	(cond 
		((null row) nil)
		((= c 0) (cons v (cdr row)))
		(t (cons (car row) (findInRow (cdr row) (- c 1) v)))
	)
)

; set-square is a helper function for next-states. It takes in state s, a row number r, a column number c,
; and a new integer v to replace the value of s(r,c). set-square calls findInRow for the row designated by r,
; and builds the state s with the swapped value. It performs the same operations as findInRow except on lists not atoms.

(defun set-square (s r c v)
	(cond ((null s) nil)
		((= r 0) (cons (findInRow (car s) c v) (cdr s)))
		(t (cons (car s) (set-square (cdr s) (- r 1) c v)))
	)
)

; try-move is a helper function for next-states. It takes a state s, a column number c, a row number r,
; a column number of a move in the assigned direction newc, a row number of a move in the assigned direction newr 
; and a direction d. The directions have been arbitrarily assigned a number where up = 1, right = 2, down = 3, and
; left = 4. try-move returns the state that results after moving the keeper in state s in the direction d. It uses
; get-square to check the value of the square in the direction d and to "look ahead" in cases where a square 2 moves
; in this direction causes a constraint. It then uses set-square to change the value of the square in the direction d
; as well as any square affected by this move. If the move cannot be performed, it returns nil.
(defun try-move (s c r newc newr d)
	(cond 
		; if square ahead is wall, return nil
		((= (get-square s newr newc) 1) nil)
		;if square ahead is a box
		((= (get-square s newr newc) 2) 
			;if moving up
			(cond ((= d 1) (if (or (= (get-square s (- newr 1) newc) 0) (= (get-square s (- newr 1) newc) 4))
								(cond ((= (get-square s (- newr 1) newc) 0)
										(let ((s11 (set-square s (- newr 1) newc 2)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									((= (get-square s (- newr 1) newc) 4) 
										(let ((s11 (set-square s (- newr 1) newc 5)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									)
								nil))
			;if moving right
				((= d 2) (if (or (= (get-square s newr (+ newc 1)) 0) (= (get-square s newr (+ newc 1)) 4))
								(cond ((= (get-square s newr (+ newc 1)) 0)
										(let ((s11 (set-square s newr (+ newc 1) 2)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									((= (get-square s newr (+ newc 1)) 4) 
										(let ((s11 (set-square s newr (+ newc 1) 5)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									)
								nil))
			;if moving down
				((= d 3) (if (or (= (get-square s (+ newr 1) newc) 0) (= (get-square s (+ newr 1) newc) 4))
								(cond ((= (get-square s (+ newr 1) newc) 0)
										(let ((s11 (set-square s (+ newr 1) newc 2)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									((= (get-square s (+ newr 1) newc) 4) 
										(let ((s11 (set-square s (+ newr 1) newc 5)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									)
								nil))
			;if moving left
				((= d 4) (if (or (= (get-square s newr (- newc 1)) 0) (= (get-square s newr (- newc 1)) 4))
								(cond ((= (get-square s newr (- newc 1)) 0)
										(let ((s11 (set-square s newr (- newc 1) 2)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									((= (get-square s newr (- newc 1)) 4) 
										(let ((s11 (set-square s newr (- newc 1) 5)))
												(let ((s22 (set-square s11 newr newc 3)))
														(set-square s22 r c 0))))
									)
								nil))
			)
		)
		;if the box in that direction was a goal, then set new square to keeper + goal
		((= (get-square s newr newc) 4) 
			(let ((s11 (set-square s newr newc 6)))
					(set-square s11 r c 0)))
		;if the box in that direction was a goal + box, do not move in that direction
		((= (get-square s newr newc) 5) 
			(cond 
				;if moving up
				((= d 1) 
					(let ((s11 (set-square s (- newr 1) newc 2)))
						(let ((s22 (set-square s11 newr newc 6)))
							(set-square s22 r c 0))))
			;if moving right
				((= d 2) 
					(let ((s11 (set-square s newr (+ newc 1) 2)))
						(let ((s22 (set-square s11 newr newc 6)))
							(set-square s22 r c 0))))
			;if moving down
				((= d 3) 
					(let ((s11 (set-square s (+ newr 1) newc 2)))
						(let ((s22 (set-square s11 newr newc 6)))
							(set-square s22 r c 0))))
			;if moving left
				((= d 4) 
					(let ((s11 (set-square s newr (- newc 1) 2)))
						(let ((s22 (set-square s11 newr newc 6)))
							(set-square s22 r c 0))))
			))
		((= (get-square s newr newc) 0)
			(if (= (get-square s r c) 6) 
				(let ((s11 (set-square s newr newc 3)))
					(set-square s11 r c 4))
				(let ((s11 (set-square s newr newc 3)))
					(set-square s11 r c 0))))
	)
)

; This function computes the trivial admissible heuristic.
;
; h0 takes in a state s and returns the constant 0 as a trivial admissible heuristic
(defun h0 (s)
	0
  )

; This function computes the number of misplaced boxes in s.
;
; h1 takes in a state s and returns the number of boxes that are not on goal positions.
; This heuristic is admissible as the heuristic will always be smaller than the true cost since it 
; is optimistic in its assumption that it only has to take the boxes into considerations.
(defun h1 (s)
	(cond ((null s) 0)
			(t (+ (count 2 (car s) :test #'equal) (h1 (cdr s))))
	)
)

; This function computes an admissible heuristic value of s. 
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the running time of a function call.
;
;
; h905351880 is a heuristic function that takes in a state s and returns the distance between
; the goal and the box. It behaves similarly to the manhattan distance heuristic as it subtracts
; the coordinates of the goal and the box. In this manner, the heuristic is admissible diagonal
; moves are not permitted in the sokoban game therefore it is always calculating the smallest distance
; needed to travel therefore it will always be smaller or equal to the true cost.


; getBoxColumn is a helper function for getBoxPosition which is in turn a helper function for h905351880.
; It takes in a row r (list) of a state and a column number col and returns the row r and the column number col 
; for either the square with the box or the square with the box on the goal

(defun getBoxColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isBox (car r)) (isBoxStar (car r)))
	       col
	     (getBoxColumn (cdr r) (+ col 1))
	     )
	   )
	)
  )

; getBoxPosition is a helper function h905351880 that takes in state s and a row number row and returns
; a list with the position of the box as (c r)

(defun getBoxPosition (s row)
  (cond ((null s) nil)
     (t (let ((x (getBoxColumn (car s) 0)))
        (if x
          (list x row)
          (getBoxPosition (cdr s) (+ row 1))
          )
        )
     )
    )
  )

; getGoalColumn is a helper function for getGoalPosition which is in turn a helper function for h905351880.
; It takes in a row r (list) of a state and a column number col and returns the row r and the column number col 
; for either the square with the goal, the box on the goal or the keeper on the goal

(defun getGoalColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isStar (car r)) (isBoxStar (car r)) (isKeeperStar (car r)))
	       col
	     (getGoalColumn (cdr r) (+ col 1))
	     )
	   )
	)
  )

; getGoalPosition is a helper function h905351880 that takes in state s and a row number row and returns
; a list with the position of the box as (c r)

(defun getGoalPosition (s row)
  (cond ((null s) nil)
     (t (let ((x (getGoalColumn (car s) 0)))
        (if x
          (list x row)
          (getGoalPosition (cdr s) (+ row 1))
          )
        )
     )
    )
  )

; h905351880 is a heuristic function that takes in a state s and is called by search algorithm a* to find
; an optimal solution for the game of sokoban. It defines the position of the goal and the box by calling the
; respective helper functions and subtracting these coordinates to find the distance between the two.
; The goal is to minimize this distance with each move until it falls to 0.
(defun h905351880 (s)
	(let* ((goalpos (getGoalPosition s 0))
		(boxpos (getBoxPosition s 0))
	 	(gc (car goalpos)) ; gc represents the column of the goal position
	 	(gr (cadr goalpos)) ; gr represents the row of the goal position
	 	(pc (car boxpos)) ; pc represents the column of the box position
	 	(pr (cadr boxpos)) ; pr represents the row of the box position
  		)
		;(print s)
		;(print goalpos)
		;(print boxpos)
		(if (equal goalpos boxpos) ; if the positions are equal, then the distance between the two is 0
			0
		(+ (abs (- gc pc)) (abs (- gr pr))) ; else calculate the distance between them by taking the absolute value of subtraction
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
