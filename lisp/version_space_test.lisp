(load "vendor/lisp-unit.lisp")
(load "version_space.lisp")

(use-package :lisp-unit)

(define-test test-generalize
  "should return an empty list if passed an empty hypothesis"
  (assert-equal '() (generalize '() '("a" "b")) )
  
  "generalizes the two passed hypotheses"
  (assert-equal '("a") (generalize '("a") '("a")) )
  (assert-equal '("a") (generalize '("_") '("a")) )
  (assert-equal '("a") (generalize '("a") '("_")) )
  (assert-equal '("*") (generalize '("a") '("b")) )
)

(define-test test-more-general?
  "should return true if the first list is equal to the second"
  (assert-true (more-general? '("a") '("a")) )
  (assert-true (not (more-general? '("a") '("b"))) )

  "should return true if the first list is more general than the second"
  (assert-true (more-general? '("a" "*" "c") '("a" "b" "c")) )
  (assert-true (not (more-general? '("a" "*" "c") '("a" "b" "d"))) )
)

(define-test test-includes?
  "should return true if both parameters are equal"
  (assert-true (includes? "a" "a"))
  (assert-true (not (includes? "a" "b")))
  
  "should return true if the first parameter is more general"
  (assert-true (includes? "*" "a"))
  (assert-true (includes? "*" "b"))
)

(define-test test-get-potential-positions
  "should return a list of positions that can be specialized"
  (assert-equal '()
    (get-potential-positions '("rund" "blau") '("rund" "gelb") '("rund" "blau"))
  )
  
  (assert-equal '(1)
    (get-potential-positions '("rund" "*") '("rund" "gelb") '("rund" "blau"))
  )
)

(define-test test-specialize
  "should return a list of specialized hypotheses"
  (assert-equal '(("rund" "blau"))
    (specialize '("rund" "*") '("rund" "gelb") '("rund" "blau"))
  )

  (assert-equal '(("rund" "*"))
    (specialize '("*" "*") '("eckig" "blau") '("rund" "blau"))
  )

  (assert-equal '(("rund" "*" "*") ("*" "*" "klein"))
    (specialize '("*" "*" "*") '("eckig" "blau" "groß") '("rund" "blau" "klein"))
  )
)

(define-test test-gen-list
  "should return a list containing the passed `item` `size` times"
  (assert-equal '("*" "*" "*") (gen-list "*" 3) )
)

(define-test test-gen-version-space
  "should create a new version space of the specified `size`"
  (assert-equal '((("*" "*" "*")) (("_" "_" "_")))
    (gen-version-space 3)
  )
)

(run-tests)