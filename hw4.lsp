;
; Graph coloring to SAT conversion
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; This function returns the index of the variable that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).

; this function takes a node index n, a color index c and the maximum color index k and returns
; a variable index that represents the constrait "node n recieves color c"
(defun node2var (n c k)
	(+ (* (- n 1) k) c)
  )

;
; this function takes a node index n, a color index c, and a maximum color index k and returns
; a clause representing the constraint "node n gets at least one color from the set {c,c+1,...,k}."
(defun at-least-one-color (n c k)
	(cond 
		((> c k) nil)
		((<= c k) (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))
  )
)

;
; this function takes in a node index n, a color index c, and a maximum color index k and returns
; a list of clauses representing the constraint "node n gets at most one color from the set {c,c+1,...,k}."
; this constraint could be written as "color c implies not color (c+1)" all the way to k, which can
; translate to "((not c) or not (c+1))" all the way to k in CNF form
(defun at-most-one-color (n c k)
	(cond 
		((> c k) nil)
		((< c k) (append (one-color n c (+ c 1) k) (at-most-one-color n (+ c 1) k)))
	)
)

; helper function for at-most-one-color
(defun one-color (n start-c current-c k)
	(cond 
		((> current-c k) nil)
		((<= current-c k) (cons (list (* -1 (node2var n start-c k)) (* -1 (node2var n current-c k))) (one-color n start-c (+ current-c 1) k)))
		)
	)

;
; this function takes a node index n and the maximum color index k and returns
; a list of clauses that constrains node n to be colored with exactly one color from
; set {1,2,...,k}, this is defined as an 'and' statement for at-least-one-color and at-most-one-color
(defun generate-node-clauses (n k)
	(cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
  )

;
; this function takes an edge e defined as a pair (x y) and a maximum color index
; and returns a list of clauses that prohibit nodes x and y from having the same color index,
; this could be written as color "c for e1 implies not color c for e2"
; which translates to (not e1 or not e2) for color c all the way to k in CNF form
(defun generate-edge-clauses (e k)
	(let ((e1 (first e))
		  (e2 (second e)))
		(edge-checker e1 e2 1 k)
  )
)

; helper function for generate-edge-clauses
(defun edge-checker (e1 e2 c k)
	(cond 
		((> c k) nil)
		((<= c k) (cons (list (* -1 (node2var e1 c k)) (* -1 (node2var e2 c k))) (edge-checker e1 e2 (+ c 1) k)))
		)
	)

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
