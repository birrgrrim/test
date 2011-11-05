(defmacro our-let (args &body body)
  `((lambda ,(mapcar #'car args) ,@body)
    ,@(mapcar #'cadr args)))

(defmacro our-let (args &body body)
    `((lambda ,(mapcar #'car args) ,@body)
      ,@(mapcar (lambda (x)
                  (if (cdr x)
                    (cadr x)
                    1))
                args)))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; -----

(defun buildmacro (args syms)
  (if args
    (let ((sym (gensym)))
      `(let* ((,sym ,(car args))
              (it ,sym))
         ,(buildmacro (cdr args)
                      (append syms 
                              (list sym)))))
    `(+ ,@syms)))

(defmacro a+ (&rest args)
  (buildmacro args nil))

;; -----

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
