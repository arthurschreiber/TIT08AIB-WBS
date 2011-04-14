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

(run-tests)