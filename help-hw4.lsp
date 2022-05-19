(defun node2var (n c k)
  (+ (* (- n 1) k) c)
)

(defun at-least-one-color (n c k)
  (cond ((< k 1) NIL) ; k can't be less than 1
        ((< k c) NIL) ; k can't be less than c
        ((= k c) (list (node2var n c c)))
        (t (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k)))
  )
)

; This function returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
(defun at-most-one-color (n c k)
	(cond	((< k 1) NIL) ; k can't be less than 1
        ((< k c) NIL) ; k can't be less than c
        ((= k c) (list (list (* n c) (- (* n c))))) ; Only one choice for color. Can be either n*c or -n*c but not both
		    (t (at-most-helper (at-least-one-color n c k)))
	)
)

; helper function that appends permutations together
(defun at-most-helper (l)
  (cond ((< (length l) 2) NIL) ; if l is an single element atom
        (t (append (permutations (- (car l)) (cdr l)) (at-most-helper (cdr l))))
  )
)

(defun permutations (x l)
  (cond
    ((NULL l) NIL) ; empty list
    (t
      (append
        (list (list x (- (car l)))) ; list of pairs
        (permutations x (cdr l)) ;of negated literal in l w/
              ; x
      )
    )
  )
)