(defmacro unit-test (expr result)
    `(unless (equal ,expr ,result)
        (error (format 'nil "Unit test failed: ~A => ~A" ',expr ,expr))))

(unit-test (contains `(1 2 3) 1) t)
(unit-test (contains `(1 2 3) 2) t)
(unit-test (contains `(1 2 3) 3) t)
(unit-test (contains `(1 2 3) 4) nil)

(unit-test (right-diagonal `(1) 2) t)
(unit-test (right-diagonal `(1) 1) nil)
(unit-test (right-diagonal `(2 1) 2) t)
(unit-test (right-diagonal `(2 1) 1) nil)
(unit-test (right-diagonal `(2 1) 3) nil)
(unit-test (right-diagonal `(1 1 1) 1) nil)
(unit-test (right-diagonal `(1 1 1) 2) t)
(unit-test (right-diagonal `(1 1 1) 3) t)
(unit-test (right-diagonal `(1 1 1) 4) t)

(unit-test (left-diagonal 4 `(1 1 1) 1) nil)
(unit-test (left-diagonal 4 `(1 1 1) 2) nil)
(unit-test (left-diagonal 4 `(1 1 1) 3) nil)
(unit-test (left-diagonal 4 `(1 1 1) 4) nil)
(unit-test (left-diagonal 4 `(4 4 4) 1) t)
(unit-test (left-diagonal 4 `(4 4 4) 2) t)
(unit-test (left-diagonal 4 `(4 4 4) 3) t)
(unit-test (left-diagonal 4 `(4 4 4) 4) nil)
(unit-test (left-diagonal 2 `(1) 1) nil)
(unit-test (left-diagonal 2 `(2) 1) t)

(unit-test (place-queen 4 `(1) 1) nil)
(unit-test (place-queen 4 `(1) 2) nil)
(unit-test (place-queen 4 `(1) 3) `(1 3))
(unit-test (place-queen 4 `(1) 4) `(1 4))

(unit-test (add-queen 2 `()) `((1) (2)))
(unit-test (add-queen 2 `(1)) nil)
(unit-test (add-queen 2 `(2)) nil)
(unit-test (add-queen 4 `(3 1 4)) `((3 1 4 2)))
(unit-test (add-queen 4 `(2 4 1)) `((2 4 1 3)))
(unit-test (add-queens 4 `((1 3) (1 4) (2 4) (3 1) (4 1) (4 2)))
    `((1 4 2) (2 4 1) (3 1 4) (4 1 3)))
(unit-test (add-queens 4 `((1 4 2) (2 4 1) (3 1 4) (4 1 3))) 
    `((2 4 1 3) (3 1 4 2)))

; final test cases
(unit-test (queens 2) nil)
(unit-test (queens 3) nil)
(unit-test (length (queens 4)) 2)
(unit-test (length (queens 5)) 10)
(unit-test (length (queens 6)) 4)
(unit-test (length (queens 7)) 40)
(unit-test (length (queens 8)) 92)

(print "all tests passed :)")