(defmacro nil! (var)
  (list 'setq var nil))

(defmacro nil! (var)
  `(setq ,var nil))

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(defmacro my-dolist ((var lst) &body body)
  `(mapcar #'(lambda (,var)
               ,@body)
           ,lst))

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
     ((> ,var limit))
     ,@body))

;; right version

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
       ((> ,var ,gstop))
       ,@body)))
