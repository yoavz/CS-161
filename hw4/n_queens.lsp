; top-level queens function
(defun queens (n) (queens-aux n 1 `(())))
(defun queens-aux (n curr_row boards)
  (if (> curr_row n) boards
    (queens-aux n (+ 1 curr_row) (add-queens n boards))))

(defun add-queens (n boards)
  (if (null boards) `()
    (append (add-queen n (car boards)) 
            (add-queens n (cdr boards)))))

; returns a list of possible next positions for the next row
(defun add-queen (n board)
  (add-queen-aux n board 1))
(defun add-queen-aux (n board col) 
  (if (> col n) nil
    (let* 
      ((new_board (place-queen n board col))
       (recurse (add-queen-aux n board (+ col 1))))
      (if (null new_board) recurse (cons new_board recurse)))))

; tries to place a queen on (row, column), returns
; the new board if successful, returns nil otherwise
(defun place-queen (n board column)
  (cond ((contains board column) nil)
        ((right-diagonal board column) nil)
        ((left-diagonal n board column) nil)
        (t (append board (list column)))))

; returns nil if the board contains a queen on col,
; and returns t otherwise
(defun contains (board col)
  (cond 
    ((null board) nil)
    ((= (car board) col) t)
    (t (contains (cdr board) col))))

; For a given board, returns true if a queen in (row, column) would be in a
; right-down diagonal of a queen already on the board. The row used is the 
; next row on the board.
(defun right-diagonal (board column)
  (rd-aux 1 board (+ (length board) 1) column))
(defun rd-aux (curr_row board row column)
  (cond ((null board) nil)
        ((= (- curr_row (car board)) (- row column)) t)
        (t (rd-aux (+ curr_row 1) (cdr board) row column))))

; Analogous to the right-diagonal function, but this time checks for a 
; left-diagonal
(defun left-diagonal (n board column)
  (ld-aux n 1 board (+ (length board) 1) column))
(defun ld-aux (n curr_row board row column)
  (cond ((null board) nil)
        ((= (- curr_row (+ (- n (car board)) 1)) 
            (- row      (+ (- n column)      1))) t)
        (t (ld-aux n (+ curr_row 1) (cdr board) row column))))
