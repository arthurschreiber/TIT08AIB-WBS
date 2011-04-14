(load "vendor/lisp-unit.lisp")
(load "version_space.lisp")

(use-package :lisp-unit)

(define-test test-generalize
  "should return an empty list if passed an empty hypothesis"
  (assert-equal '()
    (generalize '() '("a" "b"))
  )
  
  "generalizes the two passed hypotheses"
  (assert-equal '("a")
    (generalize '("a") '("a"))
  )
      
  (assert-equal '("a")
    (generalize '("_") '("a"))
  )
  
  (assert-equal '("a")
    (generalize '("a") '("_"))
  )
    
  (assert-equal '("*")
    (generalize '("a") '("b"))
  )
)



(run-tests)