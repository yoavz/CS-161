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
  (if (atom Tree)
      (if (null Tree) nil (list Tree))
      (append (dfs (car Tree)) (dfs (cdr Tree)))))

; DFS-DEPTH implements a depth first search of the Tree limited to a max depth.
; The implementation is very similar to the above DFS function, except that it
; must keep track of a "depth" argument to track how deep it is into the tree.
; If the search has exceeded the depth, the function immediately returns.
; Arguments:
;   1. Tree - a search tree
;   2. depth - an integer specifying the max depth
(defun DFS-DEPTH (Tree depth) 
  (if (or (<= depth 0) (atom Tree))
      (if (atom Tree) (list Tree) `())
      (if (<= (length Tree) 1) 
          (DFS-DEPTH (car Tree) (- depth 1))
          (append (DFS-DEPTH (car Tree) (- depth 1))
                  (DFS-DEPTH (cdr Tree) depth)))))

; DFID(-AUX) implement a depth-first iterative-deepening search. This is done
; by calling DFS-DEPTH with the given Tree for a depth of 1 - max-depth and 
; appending the results together. DFID returns a top-level nodes visited in the
; order of the search.
; Arguments:
;   1. Tree - a search tree
;   2. max-depth - the maximum depth the dfid search should go to
(defun DFID-AUX (Tree max-depth counter)
  (if (> counter max-depth) `()
      (append (DFS-DEPTH Tree counter) 
              (DFID-AUX Tree max-depth (+ counter 1)))))
(defun DFID (Tree max-depth) 
  (DFID-AUX Tree max-depth 0))

; Problem 3

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the other.
; There must be at least one person in the boat to cross the river. There can
; never be more cannibals on one side of the river than missionaries. If there
; are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list (MISSIONARIES
; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
; if it is on the east side and NIL if on the west side. MISSIONARIES and
; CANNIBALS represent the number of missionaries and cannibals on the same side
; as the boat. Thus, the initial state for this problem is (3 3 T) (three
; missionaries, three cannibals, and the boat are all on the east side of the
; river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal `(3 3 NIL) s))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
    ; define the number of missionaries and cannibals on each side
    (let* ((otherM (- (car s) m))  
           (otherC (- (car (cdr s)) c))
           (boatM (- 3 otherM))
           (boatC (- 3 otherC)))
            ; if there are more cannibals than missionaries, return nil 
      (cond ((or (and (> boatC boatM) (> boatM 0))
                 (and (> otherC otherM) (> otherM 0))) NIL)
            ; if there are an invalid number of people on either side, return nil
            ((or (< otherM 0) (< otherC 0) (< boatM 0) (< boatC 0)) NIL)
            ; otherwise, return the counts and flip the boat side
            (t (list (list boatM boatC (not (car (last s)))))))))


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
  (append (next-state s 1 0) ; move one missionary
          (next-state s 2 0) ; move two missionaries
          (next-state s 0 1) ; move one cannibal
          (next-state s 0 2) ; move two cannibals
          (next-state s 1 1))) ; move one missionary and one cannibal

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by MC-DFS (STATES). It returns T if S is a member of
; STATES and NIL otherwise.
(defun on-path (s states)
  (if (not states) NIL
      (or (equal s (car states)) (on-path s (cdr states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: the path
; from from the initial state to the current state (PATH), and the legal
; successor states to the last state on PATH (STATES). PATH is a first-in
; first-out list of states; that is, the first element is the initial state for
; the current search and the last element is the most recent state explored.
; MULT-DFS does a depth-first search on each element of STATES in turn. If any
; of those searches reaches the final state, MULT-DFS returns the complete path
; from the initial state to the goal state. Otherwise, it returns NIL.
(defun mult-dfs (states path)
  (cond 
    ; if no states left to process, we've reached a dead end
    ((not states) nil)
    ; try dfs on the first state and return if it works
    ((mc-dfs (car states) path) (mc-dfs (car states) path))
    ; try the rest of the states
    (t (mult-dfs (cdr states) path))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond 
    ; if the final state has been reached return
    ((final-state s) (append path (list s)))
    ; check if the current path is a loop
    ((on-path s path) nil)
    ; otherwise, do a dfs on all possible successor states
    (t (mult-dfs (succ-fn s) (append path (list s))))))
