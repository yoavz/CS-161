; Yoav Zimmerman (304125151)
; CS 161, Fall 2015

; QUEENS returns a solution to the N-Queens problem
; Arguments:
;   n - the width and height of the solution board
(defun queens (n) (queen-backtrack n `()))

; For the following functions, we define a "board" as
; a list of integers. The index of the integer represents
; the row position of a queen while it's value represents
; it's column position. For example, the board (3 1 4 2)
; indicates a queen at column 3 at the first row, a queen
; at column 1 at the second row, and so on.

; QUEEN-BACKTRACK returns an n-queens solution constrained to the given board.
; It does this by checking if the board is the proper length, then immediately
; calling queen-backtrack-aux.
; Arguments:
;   n - the width and height of the solution board 
;   board - the board on which the queen(s) will placed
(defun queen-backtrack (n board)
  (if (= (length board) n) board
    (queen-backtrack-aux n board 1)))

; QUEEN-BACKTRACK-AUX attempts to place a queen on the next row of the board,
; starting at the given column. If it can place a queen at the column and that
; column leads to a solution, it returns the solution. Otherwise, it tries to 
; place a queen on the next column. If no queen can be placed on the next row,
; it returns nil.
; Arguments:
;   n - the width and height of the solution board 
;   board - the board on which the queen(s) will placed
;   column - the column to try and place the queen on.
(defun queen-backtrack-aux (n board col) 
  (if (> col n) nil
    (let* 
      ((new_board (place-queen n board col))
      (new_path (if (null new_board) nil (queen-backtrack n new_board))))
      (if (null new_path) (queen-backtrack-aux n board (+ col 1)) new_path))))

; PLACE-QUEEN attempts to place a queen on the next row of the board at the
; column specified. If the queen can legally be placed there, this function
; returns the new board. If not, the function returns nil.
; Arguments: 
;   n - the width and height of the solution board 
;   board - the board on which a queen will placed
;   column - the column the queen should try to be placed on.
(defun place-queen (n board column)
  (cond ((contains board column) nil)
        ((right-diagonal board column) nil)
        ((left-diagonal n board column) nil)
        (t (append board (list column)))))

; CONTAINS returns true if the board contains a queen at the specified column
; position and nil otherwise
; Arguments: 
;   board - the board being checked
;   col - the column being checked for.
(defun contains (board col)
  (cond 
    ((null board) nil)
    ((equal (car board) col) t)
    (t (contains (cdr board) col))))

; RIGHT-DIAGONAL checks if a queen can be placed on the next row at the
; specified column with respect to the diagonal going down and right. If a
; queen is hitting an existing queens right diagonal, this function returns
; true, otherwise it returns nil.
; Arguments: 
;   board - the board being checked
;   col - the column the queen will be placed at, on the next row
(defun right-diagonal (board column)
  (rd-aux 1 board (+ (length board) 1) column))
(defun rd-aux (curr_row board row column)
  (cond ((null board) nil)
        ((= (- curr_row (car board)) (- row column)) t)
        (t (rd-aux (+ curr_row 1) (cdr board) row column))))

; LEFT-DIAGONAL is analagous to RIGHT-DIAGONAL, except it checks for a conflict
; with a previous queens diagonal going down and left. If it finds a conflict, it
; returns true, otherwise it returns nil.
; Arguments: 
;   board - the board being checked
;   col - the column the queen will be placed at, on the next row
(defun left-diagonal (n board column)
  (ld-aux n 1 board (+ (length board) 1) column))
(defun ld-aux (n curr_row board row column)
  (cond ((null board) nil)
        ((= (- curr_row (+ (- n (car board)) 1)) 
            (- row      (+ (- n column)      1))) t)
        (t (ld-aux n (+ curr_row 1) (cdr board) row column))))

; The following code finds ALL solutions to the N-Queens problem... I implemented
; it first before I realized the spec only asked for one solution ;)

; QUEENS-ALL/AUX return all solutions to the N-Queens problem. They do this by
; iterating through rows 1-n and calling add-queens on each solution board of
; the previous rows add-queens call.
; Arguments:
;   n - the width and height of the solution board
(defun queens-all (n) (queens-aux n 1 `(())))
(defun queens-aux (n curr_row boards)
  (if (> curr_row n) boards
    (queens-aux n (+ 1 curr_row) (add-queens n boards))))

; ADD-QUEENS tries to add a queen to the next row of each solution board in
; boards, by calling add-queen on it and appending all results. It returns
; a list of next possible board states from the input list of boards.
; Arguments: 
;   n - the width and height of the solution board 
;   boards - a list of boards
(defun add-queens (n boards)
  (if (null boards) `()
    (append (add-queen n (car boards)) 
            (add-queens n (cdr boards)))))

; ADD-QUEEN(-AUX) tries to add a queen to the next row available in board, and
; returns a list of next possible board states. It does this by calling
; place-queen on all possible columns (1-n) for the next row. If no queens are
; possible to place on the next row, this function returns nil.
; Arguments: 
;   n - the width and height of the solution board 
;   board - the board on which a queen will placed on the next row
(defun add-queen (n board)
  (add-queen-aux n board 1))
(defun add-queen-aux (n board col) 
  (if (> col n) nil
    (let* 
      ((new_board (place-queen n board col))
       (recurse (add-queen-aux n board (+ col 1))))
      (if (null new_board) recurse (cons new_board recurse)))))
