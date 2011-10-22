(defun check-elem (elem table)
  "Check and return numberic value of element ELEM using hashtable with variables TABLE."
  (cond
    ((numberp elem) elem)
    ((symbolp elem) (gethash elem table))
    ((listp elem) (calc elem))))

(defun perform-operation (func lst &optional table)
  "Perform binary operation FUNC to list LST using variables from hashtable TABLE."
  (reduce 
    (lambda (res next)
      (funcall func res (check-elem next table)))
    (cdr lst) 
    :initial-value (check-elem (car lst) table)))

(defun calc (lst &optional table)
  "Calculate list LSP. Operations can be overloaded. Hashtable TABLE contains variables."
  (cond
    ((eq '+ (car lst)) (perform-operation #'+ (cdr lst) table))
    ((eq '* (car lst)) (perform-operation #'* (cdr lst) table))
    ((eq '- (car lst)) (perform-operation #'- (cdr lst) table))
    (t (eval lst))))

(defun make-table (lst &optional (table (make-hash-table)))
  "Create hashtable TABLE with variables from list LST."
  (progn
    (mapcar 
      (lambda (var)
        (setf (gethash (car var) table) (cadr var)))
      lst)
    table))

(defun adv-let (args expr)
  "New implementation of function LET with arguments ARGS and expression EXPR."
  (if (null args)
    (calc expr)
    (calc expr (make-table args))))
