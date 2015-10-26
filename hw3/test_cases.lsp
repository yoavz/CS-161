(defmacro unit-test (expr result)
    `(unless (equal ,expr ,result)
        (error (format 'nil "Unit test failed: ~A => ~A" ',expr ,expr))))

;; utility tests
(unit-test (get-nth `(1 2) 0) 1)
(unit-test (get-nth `(1 2) 1) 2)
(unit-test (replace-nth `(1 2) 0 5) `(5 2))
(unit-test (replace-nth `(1 2) 1 5) `(1 5))

(unit-test (get-tile p23 0 0) 1)
(unit-test (get-tile p23 1 1) 0)
(unit-test (get-tile p23 5 0) 1)
(unit-test (get-tile p23 0 6) 1)
(unit-test (get-tile p23 4 5) 5)
(unit-test (replace-tile `((0 1) (2 3)) 0 0 5) `((5 1) (2 3)))
(unit-test (replace-tile `((0 1) (2 3)) 0 1 5) `((0 1) (5 3)))
(unit-test (replace-tile `((0 1) (2 3)) 1 0 5) `((0 5) (2 3)))
(unit-test (replace-tile `((0 1) (2 3)) 1 1 5) `((0 1) (2 5)))

;; goal-test
(unit-test (goal-test p1) nil)
(unit-test (goal-test p2) nil)
(unit-test (goal-test p3) nil)
(unit-test (goal-test p5) nil)
(unit-test (goal-test p21) nil)
(unit-test (goal-test p22) nil)
(unit-test (goal-test p23) t)

;; h1
(unit-test (h1 p1) 1)
(unit-test (h1 p5) 2)
(unit-test (h1 p17) 5)

;; h304125151
(unit-test (box-stuck p1 2 2) nil)
(unit-test (h304125151 p1) 1)
(unit-test (h304125151 p5) 2)
(unit-test (h304125151 p17) 5)

(print "All tests passed! :)")
