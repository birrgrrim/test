(defun remove-elem (elem lst)
  "Remove element ELEM from list LST."
  (if lst
    (if (eq elem (car lst))
      (remove-elem elem (cdr lst))
      (cons (car lst) (remove-elem elem (cdr lst))))
    nil))

(defun subtract (a b)
  "Subtract elements of list A from list B."
  (if a
    (subtract (cdr a) (remove-elem (car a) b))
    b))