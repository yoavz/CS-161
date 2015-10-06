(defun TREE-CONTAINS (N Tree)
  (cond ((atom Tree) 
         (= N Tree))  
        (t (let* ((m (car (cdr Tree))))
           (cond ((= N m) t)
                 ((> N m) (TREE-CONTAINS N (car (cdr (cdr Tree)))))
                 (t (TREE-CONTAINS N (car Tree))))))))

(defun TREE-MAX (Tree)
  (cond ((atom Tree) Tree)
        (t (TREE-MAX (car (cdr (cdr Tree)))))))

(defun TREE-ORDER (Tree)
  (cond ((atom Tree) (list Tree))
        (t (append (TREE-ORDER (car Tree)) 
                   (cons (car (cdr Tree)) (TREE-ORDER (car (cdr (cdr Tree)))))))))

(defun SUB-LIST (L start len)
  (cond ((= start 0)
         (cond ((>= len (length L)) L)
               ((= len 0) `())
               (t (cons (car L) (SUB-LIST (cdr L) 0 (- len 1))))))
        (t (SUB-LIST (cdr L) (- start 1) len))))

(defun SPLIT-LIST (L)
  (let* ((len (length L)))
    (cond ((evenp len) 
           (list (SUB-LIST L 0 (/ len 2)) 
                 (SUB-LIST L (/ len 2) (/ len 2))))
          ((oddp len) 
           (list (SUB-LIST L 0 (/ (- len 1) 2)) 
                 (SUB-LIST L (/ (- len 1) 2) (/ (+ len 1) 2)))))))

(defun BTREE-HEIGHT (Tree)
  (cond 
    ((atom Tree) 0)
    (t 
     (let* 
       ((left_height (BTREE-HEIGHT (car Tree)))
        (right_height (BTREE-HEIGHT (car (cdr Tree)))))
       (+ 1 (cond ((> left_height right_height) left_height) (t right_height)))))))

(defun LIST2BTREE (L)
  (cond 
    ((<= (length L) 1) (car L))
    (t 
     (let* 
       ((split (SPLIT-LIST L)))
       (list (LIST2BTREE (first split)) (LIST2BTREE (second split)))))))

(defun BTREE2LIST (Tree)
  (cond
    ((null Tree) NIL) 
    ((atom Tree) (list Tree))
    (t (append (BTREE2LIST (first Tree)) (BTREE2LIST (second Tree))))))
