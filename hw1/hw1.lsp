; Homework #1 for CS 161, Fall 2015
; Author: Yoav Zimmerman (304125151)

; For the following three functions, an ordered tree is either a number or a 
; list (L m R), with the following properties:
;   1. L and R are ordered trees
;   2. m is a number
;   3. all numbers in L are smaller than m.
;   4. all numbers in R are larger than m.

; TREE-CONTAINS returns true when N is a member of Tree and returns false otherwise
; Arguments: 
;   N: a number 
;   Tree: a ordered tree (see definition above)
(defun TREE-CONTAINS (N Tree)
  (cond ((atom Tree) 
         (= N Tree))  
        (t 
          (let* ((m (car (cdr Tree))))
            (cond ((= N m) t)
                  ((> N m) (TREE-CONTAINS N (car (cdr (cdr Tree)))))
                  (t (TREE-CONTAINS N (car Tree))))))))

; TREE-MAX returns the maximum number appearing in Tree
; Arguments:
;   Tree: an ordered tree
(defun TREE-MAX (Tree)
  (cond ((atom Tree) Tree)
        (t (TREE-MAX (car (cdr (cdr Tree)))))))

; TREE-ORDER returns an in-order list of the numbers appearing in Tree
; Arguments:
;   Tree: an ordered tree
(defun TREE-ORDER (Tree)
  (cond ((atom Tree) (list Tree))
        (t (append (TREE-ORDER (car Tree)) 
                   (cons (car (cdr Tree)) (TREE-ORDER (car (cdr (cdr Tree)))))))))

; SUB-LIST returns the sub-list of L starting at position start and having 
; length len
; Arguments:
;   L: a list of elements
;   start: a non-negative integer
;   len: a non-negative integer
(defun SUB-LIST (L start len)
  (cond ((= start 0)
         (cond ((>= len (length L)) L)
               ((= len 0) `())
               (t (cons (car L) (SUB-LIST (cdr L) 0 (- len 1))))))
        (t (SUB-LIST (cdr L) (- start 1) len))))

; SPLIT-LIST takes in a list L and returns two lists L1 and L2 such that L
; is the result of appending L1 and L2 AND length of L2 minus the length of
; L1 is always 0 or 1.
; Arguments:
;   L: the list to be split
(defun SPLIT-LIST (L)
  (let* ((len (length L)))
    (cond ((evenp len) 
           (list (SUB-LIST L 0 (/ len 2)) 
                 (SUB-LIST L (/ len 2) (/ len 2))))
          ((oddp len) 
           (list (SUB-LIST L 0 (/ (- len 1) 2)) 
                 (SUB-LIST L (/ (- len 1) 2) (/ (+ len 1) 2)))))))

; For the following 3 functions, define a binary tree as an atom N or a list (L R),
; where L and R are also binary trees

; BTREE-HEIGHT returns the integer height of a binary tree Tree, which is the length
; of the longest path from the root node to the farthest leaf node
; Arguments:
;   Tree: a binary tree (see definition above)
(defun BTREE-HEIGHT (Tree)
  (cond 
    ((atom Tree) 0)
    (t 
     (let* 
       ((left_height (BTREE-HEIGHT (car Tree)))
        (right_height (BTREE-HEIGHT (car (cdr Tree)))))
       (+ 1 (cond ((> left_height right_height) left_height) (t right_height)))))))

; LIST2BTREE returns a binary tree such that the tree leaves are elements of the
; input argument leaves. For any internal node in the binary tree, the number of
; leaves in it's right branch minus the number of leaves in it's left is 0 or 1.
; Arguments:
;   leaves: a list to be converted into a binary tree
(defun LIST2BTREE (leaves)
  (cond 
    ((<= (length leaves) 1) (car leaves))
    (t 
     (let* 
       ((split (SPLIT-LIST leaves)))
       (list (LIST2BTREE (first split)) (LIST2BTREE (second split)))))))

; BTREE2LIST returns a list of atoms constructed from the leaves of Tree 
; Arguments:
;   Tree: a binary tree to be converted into a list
(defun BTREE2LIST (Tree)
  (cond
    ((null Tree) NIL) 
    ((atom Tree) (list Tree))
    (t (append (BTREE2LIST (first Tree)) (BTREE2LIST (second Tree))))))
