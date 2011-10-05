(defun reverse-list (lst &optional res)
  "Reverse list LST."
  (if lst
    (reverse-list (cdr lst) (cons (car lst) res))
    res))

(defun is-palindrom (lst)
  "Check if list LST is palindrom."
  (if (equal lst (reverse-list lst))
    t
    nil))

(defun lst-length (lst &optional (len 0))
  "Length of list LST."
  (if lst
    (lst-length (cdr lst) (1+ len))
    len))

(defun remove-not-palindrom (lst)
  "Remove not-palindroms from list LST."
  (if lst
    (if (is-palindrom (car lst))
      (cons (car lst) (remove-not-palindrom (cdr lst)))
      (remove-not-palindrom (cdr lst)))
    nil))

(defun max-length-sublist (lst &optional res)
  "Find maximal sublist of list LST."
  (if lst
    (if (> (lst-length (car lst)) (lst-length res))
      (max-length-sublist (cdr lst) (car lst))
      (max-length-sublist (cdr lst) res))
    res))

(defun max-list-palindrom (lst)
  "Find maximal palindrom sublist of list LST."
  (max-length-sublist (remove-not-palindrom lst)))
