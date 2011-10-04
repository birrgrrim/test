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

(defun flatten (lst &optional res)
  (if lst
    (if (atom lst)
      (cons-end res lst)
      (prog2 (flatten (car lst) res) (flatten (cdr lst) res)))
    res))

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

(defun advanced-append (&rest lsts)
  nil)
