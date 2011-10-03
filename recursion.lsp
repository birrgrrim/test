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
  ""
  (if lst
    (cons (car lst) (add-el-to-list-end el (cdr lst)))
    (list el)))

(defun before-down (el lst &optional (res nil))
  ""
  (if lst
    (if (eq (car lst) el)
      (add-el-to-list-end (car lst) res)
      (before-down el (cdr lst) (add-el-to-list-end (car lst) res)))
    nil))

;(my-append '(1 2) '(4 5)) -> (1 2 3 4)
;(my-append '(1) nil) -> (1)
(defun my-last (lst)
  (if (cdr lst)
    (my-last (cdr lst))
    (car lst)))

(defun my-but-last (lst)
  (if (cdr lst)
    (cons (car lst) (my-but-last (cdr lst)))
    nil))

(defun my-append-down-with-last (lst1 lst2)
  (if lst1
    (my-append-down-with-last (my-but-last lst1) (cons (my-last lst1) lst2))
    lst2))

;-----

(defun my-reverse (lst)
  (if lst
    (add-el-to-list-end (car lst) (my-reverse (cdr lst)))
    nil))

(defun my-append-down-with-reverse-inner (lst1 lst2)
  (if lst1
    (my-append-down-with-reverse-inner (cdr lst1) (cons (car lst1) lst2))
    lst2))

(defun my-append-down-with-reverse (lst1 lst2)
  (my-append-down-with-reverse-inner (my-reverse lst1) lst2))

;-----

(defun my-append-up (lst1 lst2)
  (if (cdr lst1)
    (cons (car lst1) (my-append-up (cdr lst1) lst2))
    (cons (car lst1) lst2)))

;(remove-dublicates '(1 2 3 4)) -> (1 2 3 4)
;(remove-dublicates '(1 2 1 2)) -> (1 2)
;(remove-dublicates '(1 2 3 1)) -> (1 2 3)
(defun my-remove-duplicates-down (lst)
  nil)

;(substruct-sets '(1 2 3) '(1 2)) -> (3)
;(substruct-sets '(1 2 3) '(1 2 3)) -> nil
;(substruct-sets '(1 2) '(3 4)) -> (1 2)
(defun substruct-sets (set1 set2)
  nil)

;(add-sets '(1 2 3) '(1 2)) -> (1 2 3)
;(add-sets '(1 2 3) '(4 5)) -> (1 2 3 4 5)
;(add-sets '(1 2) '(1 3)) -> (1 2 3)
(defun add-sets (set1 set2)
  nil)

;multiply-sets '(1 2 3) '(1 2)) -> (1 2)
;multiply-sets '(1 2) '(3 4)) -> nil
;multiply-sets '(1 2) '(1 3)) -> (1)
(defun multiply-sets (set1 set2)
  nil)

