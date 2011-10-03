;(before 1 '(1 2 3)) -> (1)
;(before 2 '(1 2 3)) -> (1 2)
;(before 10 '(1 2 3)) -> nil
(defun before (el lst)
  nil)

;(my-append '(1 2) '(4 5)) -> (1 2 3 4)
;(my-append '(1) nil) -> nil
(defun my-append (lst1 lst2)
  nil)

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

