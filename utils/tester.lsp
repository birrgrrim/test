(defstruct test-case
  input
  output)

(defun test-one (func tc)
  (equalp
    (test-case-output tc)
    (apply func (test-case-input tc))))


(defun test (func test-cases name)
  (let ((passed
              (reduce
                #'(lambda (res tc)
                    (if (test-one func tc)
                      (1+ res)
                      (progn 
                        (format 
                          t 
                          "  Function ~A with ~A input returns ~A instead of ~A" 
                          name 
                          (test-case-input tc)
                          (apply func (test-case-input tc))
                          (test-case-output tc))
                        res)))
                test-cases
                :initial-value 0))
        (all (length test-cases)))
     (format t "~A: ~A/~A PASSED~%" name passed all)))

   
