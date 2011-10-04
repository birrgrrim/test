(load "tester.lsp")

(setq before-test-cases
  (list
    (make-test-case :input '(1 (1 2 3)) :output '(1))
    (make-test-case :input '(2 (1 2 3)) :output '(1 2))
    (make-test-case :input '(nil (1 nil 3)) :output '(1 nil))
    (make-test-case :input '(2 (nil 2 nil)) :output '(nil 2))
    (make-test-case :input '(nil (1 2 nil)) :output '(1 2 nil))
    (make-test-case :input '(10 (1 2 3)) :output nil)))

(setq append-test-cases
  (list
    (make-test-case :input '((1 2) (3 4)) :output '(1 2 3 4))
    (make-test-case :input '(nil nil) :output nil)
    (make-test-case :input '((nil) nil) :output '(nil))
    (make-test-case :input '((1) nil) :output '(1))))

(setq remove-dups-test-cases
  (list
    (make-test-case :input '((1 2 3 4)) :output '(1 2 3 4))
    (make-test-case :input '((1 2 3 1)) :output '(1 2 3))
    (make-test-case :input '(nil) :output nil)
    (make-test-case :input '((1 2 1 2)) :output '(1 2))))

(setq subsets-test-cases
  (list
    (make-test-case :input '((1 2 3) (1 2)) :output '(3))
    (make-test-case :input '((1 2 3) (1 2 3)) :output nil)
    (make-test-case :input '(nil (1 2 3)) :output nil)
    (make-test-case :input '((nil) nil) :output '(nil))
    (make-test-case :input '((1 2 3) nil) :output '(1 2 3))
    (make-test-case :input '(nil nil) :output nil)
    (make-test-case :input '((1 2) (3 4)) :output '(1 2))))

(setq addsets-test-cases
  (list
    (make-test-case :input '((1 2 3) (1 2)) :output '(1 2 3))
    (make-test-case :input '(nil (1 2 3)) :output '(1 2 3))
    (make-test-case :input '((1 2 3) nil) :output '(1 2 3))
    (make-test-case :input '(nil nil) :output nil)
    (make-test-case :input '((nil) nil) :output '(nil))
    (make-test-case :input '((1 2 3) (4 5)) :output '(1 2 3 4 5))
    (make-test-case :input '((1 2) (1 3)) :output '(1 2 3))))

(setq mulsets-test-cases
  (list
    (make-test-case :input '(nil (1 2 3)) :output nil)
    (make-test-case :input '((1 2 3) nil) :output nil)
    (make-test-case :input '(nil nil) :output nil)
    (make-test-case :input '((nil) (nil)) :output '(nil))
    (make-test-case :input '((1 2 3) (1 2)) :output '(1 2))
    (make-test-case :input '((1 2) (4 5)) :output nil)
    (make-test-case :input '((1 2) (1 3)) :output '(1))))

;-----

;(before 1 '(1 2 3)) -> (1)
;(before 2 '(1 2 3)) -> (1 2)
;(before 10 '(1 2 3)) -> nil
(defun member-of-list (el lst)
  "Returns T if element el is a member of a list lst."
  (if lst
    (if (eq (car lst) el)
      T
      (member-of-list el (cdr lst)))
    nil))

(defun before-up-inner (el lst)
  "Returns list of elements from list lst before element el."
  (if (eq (car lst) el)
    (cons (car lst) nil)
    (cons (car lst) (before-up-inner el (cdr lst)))))

(defun before-up (el lst)
  "Calls function before if element el is a member of a list lst."
  (if (member-of-list el lst)
    (before-up-inner el lst)
    nil))

;-----

(defun add-el-to-list-end (el lst)
  "Add element to list end."
  (if lst
    (cons (car lst) (add-el-to-list-end el (cdr lst)))
    (list el)))

(defun before-down (el lst &optional (res nil))
  "Returns list of elements from list lst before element el."
  (if lst
    (if (eq (car lst) el)
      (add-el-to-list-end (car lst) res)
      (before-down el (cdr lst) (add-el-to-list-end (car lst) res)))
    nil))

;(my-append '(1 2) '(3 4)) -> (1 2 3 4)
;(my-append '(1) nil) -> (1)
(defun my-last (lst)
  "Returns last element of the list."
  (if (cdr lst)
    (my-last (cdr lst))
    (car lst)))

(defun my-but-last (lst)
  "Returns list without last element."
  (if (cdr lst)
    (cons (car lst) (my-but-last (cdr lst)))
    nil))

(defun my-append-down-with-last (lst1 lst2)
  "Appends two lists using last element."
  (if lst1
    (my-append-down-with-last (my-but-last lst1) (cons (my-last lst1) lst2))
    lst2))

;-----

(defun my-reverse (lst)
  "Returns reversed list."
  (if lst
    (add-el-to-list-end (car lst) (my-reverse (cdr lst)))
    nil))

(defun my-append-down-with-reverse-inner (lst1 lst2)
  "Appends reversed list lst1 with list lst2."
  (if lst1
    (my-append-down-with-reverse-inner (cdr lst1) (cons (car lst1) lst2))
    lst2))

(defun my-append-down-with-reverse (lst1 lst2)
  "Calls function append with reverted list."
  (my-append-down-with-reverse-inner (my-reverse lst1) lst2))

;-----

(defun my-append-up (lst1 lst2)
  "Appends two lists."
  (if lst1
    (cons (car lst1) (my-append-up (cdr lst1) lst2))
    lst2))

;(remove-dublicates '(1 2 3 4)) -> (1 2 3 4)
;(remove-dublicates '(1 2 1 2)) -> (1 2)
;(remove-dublicates '(1 2 3 1)) -> (1 2 3)
(defun remove-elem (el lst)
  "Revome element from the list."
  (if lst
    (if (eq (car lst) el)
      (remove-elem el (cdr lst))
      (cons (car lst) (remove-elem el (cdr lst))))
    nil))

(defun my-remove-duplicates-down (lst &optional res)
  "Remove duplicates."
  (if lst
    (cons (car lst) (my-remove-duplicates-down (remove-elem (car lst) (cdr lst)) res))
    res))

;-----

(defun my-remove-duplicates-up (lst)
  "Remove duplicates."
  (if lst
    (cons (car lst) (remove-elem (car lst) (my-remove-duplicates-up (cdr lst))))
    nil))

;(substruct-sets '(1 2 3) '(1 2)) -> (3)
;(substruct-sets '(1 2 3) '(1 2 3)) -> nil
;(substruct-sets '(1 2) '(3 4)) -> (1 2)
(defun substruct-sets-down (set1 set2)
  "Lists substraction."
  (if set2
    (substruct-sets-down (remove-elem (car set2) set1) (cdr set2))
    set1))

;-----

(defun substruct-sets-up (set1 set2)
  "List substraction."
  (if set1
    (if (member-of-list (car set1) set2)
      (substruct-sets-up (cdr set1) set2)
      (cons (car set1) (substruct-sets-up (cdr set1) set2)))
    nil))

;(add-sets '(1 2 3) '(1 2)) -> (1 2 3)
;(add-sets '(1 2 3) '(4 5)) -> (1 2 3 4 5)
;(add-sets '(1 2) '(1 3)) -> (1 2 3)
(defun add-sets-down (set1 set2)
  "Add two lists."
  (if set2
    (if (member-of-list (car set2) set1)
      (add-sets-down set1 (cdr set2))
      (add-sets-down (add-el-to-list-end (car set2) set1) (cdr set2)))
    set1))

;-----

(defun add-sets-up (set1 set2)
  "Add two lists."
  (my-remove-duplicates-up (my-append-up set1 set2)))

;multiply-sets '(1 2 3) '(1 2)) -> (1 2)
;multiply-sets '(1 2) '(3 4)) -> nil
;multiply-sets '(1 2) '(1 3)) -> (1)
(defun multiply-sets-down (set1 set2 &optional (res nil))
  "Multiply two lists."
  (if set2
    (if (member-of-list (car set2) set1)
      (multiply-sets-down set1 (cdr set2) (add-el-to-list-end (car set2) res))
      (multiply-sets-down set1 (cdr set2) res))
    res))

;-----

(defun multiply-sets-up (set1 set2)
  "Multiply two lists."
  (if set1
    (if (member-of-list (car set1) set2)
      (cons (car set1) (multiply-sets-up (cdr set1) set2))
      (multiply-sets-up (cdr set1) set2))
    nil))

