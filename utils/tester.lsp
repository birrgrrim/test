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
                      res))
                test-cases
                :initial-value 0))
        (all (length test-cases)))
     (format t "~A: ~A/~A PASSED~%" name passed all)))

(defun test-all ()
    (test #'before-up before-test-cases "before-up")
    (test #'before-down before-test-cases "before-down")
    (test #'my-append-down-with-last append-test-cases "my-append-down-with-last")
    (test #'my-append-down-with-reverse append-test-cases "my-append-down-with-reverse")
    (test #'my-append-up append-test-cases "my-append-up")
    (test #'my-remove-duplicates-down remove-dups-test-cases "my-remove-duplicates-down")
    (test #'my-remove-duplicates-up remove-dups-test-cases "my-remove-duplicates-up") 
    (test #'substruct-sets-down subsets-test-cases "substruct-sets-down") 
    (test #'substruct-sets-up subsets-test-cases "substruct-sets-up")
    (test #'add-sets-down addsets-test-cases "add-sets-down")
    (test #'add-sets-up addsets-test-cases "add-sets-up")
    (test #'multiply-sets-down mulsets-test-cases "multiply-sets-down")
    (test #'multiply-sets-up mulsets-test-cases "multiply-sets-up")
    (test #'group group-test-cases "group")
    (test #'flatten flatten-test-cases "flatten")
    (test #'between between-test-cases "between")
    (test #'split split-test-cases "split")
    (test #'advanced-before abefore-test-cases "a-before")
    (test #'advanced-append aappend-test-cases "a-append"))
   
