;;
;; flattens lists
;;
;; (time (fast-flatten '(a (b (c (d (e (f (g (h (i (j (k (l (m (n (o (p (r (s (t (u (v (w (x (y (z)))))))))))))))))))))))))))
;; Real time: 5.23E-4 sec.
;; Space: 7896 Bytes
;;
;; Real time: 3.6E-5 sec.
;; Space: 7600 Bytes
;;
;; length: 10.000.000
;; depth:  1.000-
;; Real time: 24.434011 sec.
;; Run time: 24.353521 sec.
;; Space: 3655836424 Bytes
;; GC: 62, GC time: 12.13676 sec.
;;
;; + tail recursion
;; - reverse each recursive call (increases time and size) (using nreverse instead of reverse will improve result)
;; - append each recursive call (increases time and size) 
;;
(defun fast-flatten (L &optional (A nil))
	"flattens lists"
	(if (null L)
		(reverse A)
		(if (listp (car L))
			(fast-flatten (append (flat-t (car L) nil) (cdr L)) A)
			(fast-flatten (cdr L) (cons (car L) A)))))
;;
;; flattens lists
;;
;; (time (flatten '(a (b (c (d (e (f (g (h (i (j (k (l (m (n (o (p (r (s (t (u (v (w (x (y (z)))))))))))))))))))))))))))
;; Real time: 8.1E-5 sec.
;; Space: 2600 Bytes
;;
;; Real time: 1.7E-5 sec.
;; Space: 2600 Bytes
;;
;; length: 10.000.000
;; depth:  1.000-
;; Real time: 9.494925 sec.
;; Run time: 9.424589 sec.
;; Space: 1245278808 Bytes
;; GC: 28, GC time: 6.216387 sec
;;
;; + short
;; - append each recursive call
;;
(defun flatten (L)
	"flattens lists"
	(if (null L)
		nil
		(if (listp (car L))
			(append (flatten (car L)) (flatten (cdr L)))
			(cons (car L) (flatten (cdr L))))))


(defun flatten (lst) 
  (reduce 
    #'(lambda (res a) 
        (if (and (listp a) a) 
          (append res (flatten a)) 
          (append res (list a)))) 
    lst 
    :initial-value nil))
