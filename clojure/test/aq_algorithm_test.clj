(ns aq-algorithm-test (:use [clojure.test]))

(load "src/aq_algorithm")

(deftest test-select-best-generalization
  (testing "should select the least general generalization"
    (is
      (=
        (aq-algorithm/select-best-generalization '(("a" :* :*) ("b" :* :*) ("a" "b" :*)))
        '("a" "b" :*)
      )
    )
  )
)

(run-tests)
