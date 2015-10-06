 (defmacro unit-test (expr result)
   `(unless (equal ,expr ,result)
     (error (format 'nil "Unit test failed: ~A => ~A" ',expr ,expr))))
 
;; Problem 1
(unit-test (TREE-CONTAINS 3 `((1 2 3) 7 8)) t)
(unit-test (TREE-CONTAINS 4 `((1 2 3) 7 8)) nil)
(unit-test (TREE-CONTAINS 7 `((1 2 3) 7 8)) t)
(unit-test (TREE-CONTAINS 8 `((1 2 3) 7 8)) t)
 
;; Problem 2
(unit-test (TREE-MAX `((1 2 3) 7 8)) 8)
(unit-test (TREE-MAX `((1 2 3) 3 3)) 3)
(unit-test (TREE-MAX `(1 2 3)) 3)
(unit-test (TREE-MAX 3) 3)
 
;; Problem 3
(unit-test (TREE-ORDER 3) `(3))
(unit-test (TREE-ORDER `((1 2 3) 7 8)) `(1 2 3 7 8))
(unit-test (TREE-ORDER `((1 2 3) 7 (8 9 10))) `(1 2 3 7 8 9 10))
(unit-test (TREE-ORDER `(1 7 (8 9 10))) `(1 7 8 9 10))
 
;; Problem 4
(unit-test (SUB-LIST `() 0 0) `())
(unit-test (SUB-LIST `() 1 0) `())
(unit-test (SUB-LIST `(a b c d) 0 3) `(a b c))
(unit-test (SUB-LIST `(a b c d) 3 1) `(d))
(unit-test (SUB-LIST `(a b c d) 2 0) NIL)
(unit-test (SUB-LIST `((1 2) 3 4) 0 3) `((1 2) 3 4))
(unit-test (SUB-LIST `((1 2) 3 4) 1 2) `(3 4))
; start + len longer than L
(unit-test (SUB-LIST `(a b c d) 2 3) `(c d))
(unit-test (SUB-LIST `(a b c d) 3 3) `(d)) 
;  
; ;; Problem 5
(unit-test (SPLIT-LIST `(a b c d))     `((a b) (c d)))
(unit-test (SPLIT-LIST `(a b c d e))   `((a b) (c d e)))
(unit-test (SPLIT-LIST `(a b c d e f)) `((a b c) (d e f)))
(unit-test (SPLIT-LIST `()) `(() ()))
(unit-test (SPLIT-LIST `(a)) `(() (a)))
(unit-test (SPLIT-LIST `(a b)) `((a) (b)))

; ;; Problem 6
(unit-test (BTREE-HEIGHT 1)  0)
(unit-test (BTREE-HEIGHT `(1 2))  1)
(unit-test (BTREE-HEIGHT `(1 (2 3)))  2)
(unit-test (BTREE-HEIGHT `((1 2) (3 4)))  2)
(unit-test (BTREE-HEIGHT `((1 (2 3)) ((4 5) (6 7))))  3)
(unit-test (BTREE-HEIGHT `(((1 2) (3 4)) ((5 6) (7 8))))  3)

; ;; Problem 7
(unit-test (LIST2BTREE `())  nil)
(unit-test (LIST2BTREE `(1))  1)
(unit-test (LIST2BTREE `(1 2))  `(1 2))
(unit-test (LIST2BTREE `(1 2 3))  `(1 (2 3)))
(unit-test (LIST2BTREE `(1 2 3 4))  `((1 2) (3 4)))
(unit-test (LIST2BTREE `(1 2 3 4 5 6 7))  `((1 (2 3)) ((4 5) (6 7))))
(unit-test (LIST2BTREE `(1 2 3 4 5 6 7 8))  `(((1 2) (3 4)) ((5 6) (7 8))))

; ;; Problem 8
(unit-test (BTREE2LIST nil) nil)
(unit-test (BTREE2LIST 1)  `(1))
(unit-test (BTREE2LIST `(1 2))  `(1 2))
(unit-test (BTREE2LIST `(1 (2 3)))  `(1 2 3))
(unit-test (BTREE2LIST `((1 2) (3 4)))  `(1 2 3 4))
(unit-test (BTREE2LIST `((1 (2 3)) ((4 5) (6 7))))  `(1 2 3 4 5 6 7))
(unit-test (BTREE2LIST `(((1 2) (3 4)) ((5 6) (7 8))))  `(1 2 3 4 5 6 7 8))

(print "All tests passed!")
