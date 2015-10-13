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

(print "All tests passed!")
