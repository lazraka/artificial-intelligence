;;; Question 1
(defun PAD (n) 											; function PAD takes 1 integer argument n and returns the nth integer in the Padovan sequence
	(if (< n 3) 1 										; if the integer is less than 3 return 1
	(+ (PAD (- n 1)) (PAD (- n 2)) (PAD (- n 3))) 		; else sum the 3 preceeding integers in the sequence
)
)

;;;Question 2
(defun SUMS (n)          								; function SUMS takes 1 integer argument n and returns the number of additions performed in the Padovan sequence to find the nth element
	(if (< n 3) 0        								; if the argument is less than 3, no additions are performed
	(+ (SUMS (- n 1)) (SUMS (- n 2)) (SUMS (- n 3)) 2) 	; else sum the number of additions performed for the preceeding 3 integers plus 2 additions to obtain the nth element
)
)


;;;Question 3
(defun ANON (tree)										; function ANON takes 1 argument representing a tree list and returns a tree list with all symbols 0 (anonymous tree)
	(cond ((not tree) nil) 								; first case returns nil for empty argument
		((atom tree) 0)									; second case returns 0 for an atom
		(t (cons (ANON (car tree))						; third case recursively calls the function on the head and tail of the argument
				(ANON (cdr tree)))
		)
	)
)