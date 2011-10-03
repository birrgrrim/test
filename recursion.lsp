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
    (cons (car lst) (before el (cdr lst)))))

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

(defun before-down-inner (el lst &optional (res nil))
  ""
  (if (eq (car lst) el)
    (add-el-to-list-end (car lst) res)
    (before-down-inner el (cdr lst) (add-el-to-list-end (car lst) res))))

(defun before-down (el lst)
  ""
  (if (member-of-list el lst)
    (before-down-inner el lst)
    nil))

;(my-append '(1 2) '(4 5)) -> (1 2 3 4)
;(my-append '(1) nil) -> nil
(defun my-append-down (lst1 lst2)
  (if lst)

;(remove-dublicates '(1 2 3 4)) -> (1 2 3 4)
;(remove-dublicates '(1 2 1 2)) -> (1 2)
;(remove-dublicates '(1 2 3 1)) -> (1 2 3)
(defun my-remove-duplicates (lst)
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

