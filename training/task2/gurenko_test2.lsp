(defun make-adder (int)
  "Returns function that adds INT to parameter."
  (lambda (x)
    (+ int x)))

(defun my-complement (pred)
  "Returns function that is NOT PRED."
  (lambda (x)
    (if (funcall pred x)
      nil
      t)))

(defun compose (&rest func)
  "Returns function that is compose of functions."
  (lambda (lst)
    (if (and func lst)
      (reduce #'funcall func
              :initial-value lst
              :from-end t)
      nil)))
