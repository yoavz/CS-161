; Homework #2 for CS 161, Fall 2015
; Author: Yoav Zimmerman (304125151)

; A Search Tree is represented as a list in which a leaf node is represented as
; an atom, and a non-leaf node is represented by a list of it's child nodes.

; DFS implements a depth first search of the Tree. It returns a single top-level
; list of the nodes it visits. The function simply checks if the current node 
; is an atom. If it is, return that atom inside of a list. If not, recursively
; call the function on the current node's children.
; Arguments:
;   1. Tree - a search tree as specified by the definition above
(defun DFS (Tree)
  (cond ((atom Tree)
         (list Tree))
        ((<= (length Tree) 1)
         (dfs (car Tree)))
        (t 
          (append (dfs (car Tree))
                  (dfs (cdr Tree))))))


; DFS-DEPTH implements a depth first search of the Tree limited to a max depth.
; The implementation is very similar to the above DFS function, except that it
; must keep track of a "depth" argument to track how deep it is into the tree.
; If the search has exceeded the depth, the function immediately returns.
; Arguments:
;   1. Tree - a search tree
;   2. depth - an integer specifying the max depth
(defun DFS-DEPTH (Tree depth) 
  (cond ((or (<= depth 0) (atom Tree))
         (cond ((atom Tree) (list Tree))
               (t `())))
        (t
         (cond ((<= (length Tree) 1) (DFS-DEPTH (car Tree) (- depth 1)))
               (t 
                (append (DFS-DEPTH (car Tree) (- depth 1))
                        (DFS-DEPTH (cdr Tree) depth)))))))

; DFID(-AUX) implements a depth-first iterative-deepening search. This is done
; by calling DFS-DEPTH with the given Tree for a depth of 1 - max-depth and 
; appending the results together. DFID returns a top-level nodes visited in the
; order of the search.
; Arguments:
;   1. Tree - a search tree
;   2. max-depth - the maximum depth the dfid search should go to
(defun DFID-AUX (Tree max-depth counter)
  (cond ((> counter max-depth) `())
        (t (append (DFS-DEPTH Tree counter) 
                   (DFID-AUX Tree max-depth (+ counter 1))))))
(defun DFID (Tree max-depth) 
  (DFID-AUX Tree max-depth 0))
