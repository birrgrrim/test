;(group '(1 2 3 4) 2) -> ((1 2) (3 4))
;(group '(1 2 3 4 5) 2) -> ((1 2) (3 4) (5))
;(group '(1 2 3 4 5) 3) -> ((1 2 3) (4 5))
(defun group (lst dev)
  nil)

;(flatten '((1) (2 (3)) (4 (5)))) -> (1 2 3 4 5)
(defun cons-end (lst el)
  (if lst
    (cons (car lst) (cons-end (cdr lst) el))
    (list el)))

(defun flatten (lst)
  (cond 
    ((null lst) nil)
    ((atom lst) (list lst))
    (t (append
         (flatten (car lst))
         (flatten (cdr lst))))))

;(between 1 3 '(1 2 3 4)) -> (1 2 3)
;(between 3 1 '(1 2 3 4)) -> nil
;(between 1 1 '(1 2 3 4)) -> nil
;(between 1 3 '(1 2 3 1 4 3)) -> (1 2 3)
(defun between (a b lst)
  nil)

;read about lisp function "values"
;(split 2 '(1 2 3)) -> (1 2) (3)
;(split 4 '(1 2 3)) -> (1 2 3) nil
(defun split (a lst)
  nil)

;(advanced-before 2 '(1 2 3)) -> (1 2)
;(advanced-before 2 '(1 2 3) :quantity 2) -> nil
;(advanced-before 2 '(1 2 3 1 2 3) :quantity 2) -> (1 2 3 1 2)
(defun advanced-before (a lst &key (quantity 1))
  nil)

;(advanced-append '(1 2) '(3 4) '(5 6)) -> (1 2 3 4 5 6)
(defun append-two (lst1 lst2)
  (cond
    ((null lst1) lst2)
    (t (cons 
         (car lst1) 
         (append-two (cdr lst1) lst2)))))

(defun advanced-append (&rest lsts)
  (cond 
    ((null lsts) nil)
    (t (append-two (car lsts) (advanced-append (cdr lsts))))))
