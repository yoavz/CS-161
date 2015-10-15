 (defmacro unit-test (expr result)
   `(unless (equal ,expr ,result)
     (error (format 'nil "Unit test failed: ~A => ~A" ',expr ,expr))))
 
;; Problem 1
; (unit-test (DFS `()) `())
(unit-test (DFS `(A)) `(A))
(unit-test (DFS `((A))) `(A))
(unit-test (DFS `((A) B)) `(A B))
(unit-test (DFS `((A (B)) C (D))) `(A B C D))
(unit-test (DFS `(A (B (C D)) (((E))))) `(A B C D E))

;; Problem 2
(unit-test (DFS-DEPTH `(A B) 1) `(A B))
(unit-test (DFS-DEPTH `(A (B C)) 1) `(A))
(unit-test (DFS-DEPTH `((A (B)) C (D)) 0) `())
(unit-test (DFS-DEPTH `((A (B)) C (D)) 1) `(C))
(unit-test (DFS-DEPTH `((A (B)) C (D)) 2) `(A C D))
(unit-test (DFS-DEPTH `((A (B)) C (D)) 3) `(A B C D))
(unit-test (DFS-DEPTH `((A (B)) C (D)) 3) `(A B C D))
(unit-test (DFS-DEPTH `(A) 0) `())
(unit-test (DFS-DEPTH `((A)) 1) `())
(unit-test (DFS-DEPTH `((A)) 2) `(A))
(unit-test (DFID `((A (B)) C (D)) 0) `()) 
(unit-test (DFID `((A (B)) C (D)) 1) `(C)) 
(unit-test (DFID `((A (B)) C (D)) 2) `(C A C D)) 
(unit-test (DFID `((A (B)) C (D)) 3) `(C A C D A B C D)) 

(unit-test (final-state `(3 3 NIL)) t)
(unit-test (final-state `(2 3 NIL)) NIL)
(unit-test (final-state `(3 3 t)) NIL)

(unit-test (next-state `(3 3 NIL) 4 4) NIL)
(unit-test (next-state `(3 3 NIL) 0 0) `((0 0 t)))
(unit-test (next-state `(0 0 t) 1 0) NIL)
(unit-test (next-state `(3 3 t) 1 0) NIL)
(unit-test (next-state `(3 3 t) 0 1) `((0 1 NIL)))
(unit-test (next-state `(3 3 NIL) 1 1) `((1 1 t)))

(unit-test (on-path `(3 3 t) `((2 1 t) (3 1 t) (3 3 t))) t)
(unit-test (on-path `(3 3 t) `((2 1 t) (3 1 t) (3 3 nil))) nil)

(print "All tests passed!")
