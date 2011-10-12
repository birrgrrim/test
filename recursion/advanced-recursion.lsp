(load "../utils/tester.lsp")

(setq group-test-cases
  (list
    (make-test-case :input '((1 2 3) 1) :output '((1) (2) (3)))
    (make-test-case :input '((1 2 3) 0) :output nil)
    (make-test-case :input '((1 2 3) 2) :output '((1 2) (3)))
    (make-test-case :input '((1 2 nil nil) 2) :output '((1 2) (nil nil)))))

(setq flatten-test-cases
  (list
    (make-test-case :input '(((1) (2 (3)) (4 (5)))) :output '(1 2 3 4 5))
    (make-test-case :input '((nil nil nil)) :output nil)
    (make-test-case :input '(((((nil))))) :output nil)
    (make-test-case :input '(nil) :output nil)))

(setq between-test-cases
  (list
    (make-test-case :input '(1 3 (1 2 3 4)) :output '(1 2 3))
    (make-test-case :input '(1 3 (1 2 3 1 4 3)) :output '(1 2 3))
    (make-test-case :input '(1 1 (1 2 3 4)) :output nil)
    (make-test-case :input '(nil nil (nil nil nil)) :output '(nil nil))))

(setq split-test-cases
  (list
    (make-test-case :input '(2 (1 2 3)) :output (values '(1 2) '(3)))
    (make-test-case :input '(4 (1 2 3)) :output (values '(1 2 3) nil))
    (make-test-case :input '(2 (1 nil 3)) :output (values '(1 nil) '(3)))
    (make-test-case :input '(1 (1)) :output (values '(1) nil))
    (make-test-case :input '(nil nil) :output (values nil nil))))

(setq abefore-test-cases
  (list
    (make-test-case :input '(1 (1 1 3) :quantity 2) :output '(1 1))
    (make-test-case :input '(nil (nil nil nil) :quantity 3) :output '(nil nil nil))))

(setq aappend-test-cases
  (list
    (make-test-case :input '((1 2) (3 4) (5 6)) :output '(1 2 3 4 5 6))
    (make-test-case :input '((1) nil nil (2)) :output '(1 2))
    (make-test-case :input '((1) (nil) nil) :output '(1 nil))))

(setq aremove-test-cases
  (list
    (make-test-case :input '(1 (1 2 3 1 2 3)) :output '(2 3 2 3))
    (make-test-case :input '(1 (1 2 3 1 2 3) :quantity 1) :output '(2 3 1 2 3))
    (make-test-case :input '(1 (1 2 3 1 2 3) :quantity 4) :output '(2 3 2 3))
    (make-test-case :input '((1) (1 2 (1) 3 1 2 3) :quantity 4) :output '(1 2 3 1 2 3))
    (make-test-case :input '(nil (1 2 (nil) 3 nil 2 nil) :quantity 1) :output '(1 2 (nil) 3 2 nil))
    (make-test-case :input '(nil (1 2 3 1 2 3) :quantity 4) :output '(1 2 3 1 2 3))))

;(group '(1 2 3 4) 2) -> ((1 2) (3 4))
;(group '(1 2 3 4 5) 2) -> ((1 2) (3 4) (5))
;(group '(1 2 3 4 5) 3) -> ((1 2 3) (4 5))
(defun my-sublist (lst dev &optional (head nil))
  (if (and lst (/= 0 dev))
    (my-sublist (cdr lst) (1- dev) (cons (car lst) head))
    (values (reverse head) lst)))

(defun group (lst dev)
  (if (and lst (/= 0 dev))
    (multiple-value-bind (head tail)
        (my-sublist lst dev)
      (cons head (group tail dev)))
    nil))

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
  (if lst
    (if (eq a (car lst))
      (if (eq a b)
        (advanced-before b lst :quantity 2)
        (advanced-before b lst))
      (between a b (cdr lst)))
    nil))

;read about lisp function "values"
;(split 2 '(1 2 3)) -> (1 2) (3)
;(split 4 '(1 2 3)) -> (1 2 3) nil
(defun split (a lst &optional head)
  (if (and lst (/= a 0))
    (split (1- a) (cdr lst) (cons (car lst) head))
    (values (reverse head) lst)))

;(advanced-before 2 '(1 2 3)) -> (1 2)
;(advanced-before 2 '(1 2 3) :quantity 2) -> nil
;(advanced-before 2 '(1 2 3 1 2 3) :quantity 2) -> (1 2 3 1 2)
(defun advanced-before (a lst &key (res nil) (quantity 1))
  (if (= 0 quantity) 
    (reverse res)
    (if lst
      (if (eq a (car lst))
        (advanced-before a (cdr lst) :res (cons (car lst) res) :quantity (1- quantity))
        (advanced-before a (cdr lst) :res (cons (car lst) res) :quantity quantity))
      nil)))

;(advanced-append '(1 2) '(3 4) '(5 6)) -> (1 2 3 4 5 6)
(defun append-two (lst1 lst2)
  (cond
    ((null lst1) lst2)
    (t (cons 
         (car lst1) 
         (append-two (cdr lst1) lst2)))))

(defun advanced-append (&rest lsts)
  (if lsts
    (append-two (car lsts) (apply #'advanced-append (cdr lsts)))
    nil))

;(advanced-remove 1 '(1 2 3 1 2 3 1 2 3) :quantity 2) -> (2 3 2 3 1 2 3)
(defun advanced-remove-down (el lst &key (res nil) (quantity -1))
  (if lst
    (if (and (equal el (car lst)) (/= 0 quantity))
      (advanced-remove-down el (cdr lst) :res res :quantity (1- quantity))
      (advanced-remove-down el (cdr lst) :res (append res (list (car lst))) :quantity quantity))
    res))

(defun advanced-remove-up (el lst &key (quantity -1))
  (if lst
    (if (and (equal el (car lst)) (/= 0 quantity))
      (advanced-remove-up el (cdr lst) :quantity (1- quantity))
      (cons (car lst) (advanced-remove-up el (cdr lst) :quantity quantity)))
    nil))

(defun test-all ()
    (test #'group group-test-cases "group")
    (test #'flatten flatten-test-cases "flatten")
    (test #'between between-test-cases "between")
    (test #'split split-test-cases "split")
    (test #'advanced-before abefore-test-cases "a-before")
    (test #'advanced-append aappend-test-cases "a-append")
    (test #'advanced-remove-down aremove-test-cases "advanced-remove-down")
    (test #'advanced-remove-up aremove-test-cases "advanced-remove-up"))

