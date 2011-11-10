(set-macro-character #\'
                     #'(lambda (stream char)
                         (print stream)
                         (print char)
                         (list 'quote (read stream t nil t))))

(set-dispatch-macro-character #\# #\?
                              #'(lambda (stream sub-char arg)
                                  (format t "~A ~A" sub-char arg)
                                  `#'(lambda (&rest ,(gensym))
                                       ,(read stream t nil t))))

(set-dispatch-macro-character #\# #\[
                              #'(lambda (stream char1 char2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\] stream t)))
                                    (do ((i (ceiling (car pair)) (1+ i)))
                                      ((> i (floor (cadr pair)))
                                       (list 'quote (nreverse accum)))
                                      (push i accum)))))

;(set-macro-character #\]
;                     (get-macro-character #\)))
