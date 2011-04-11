(ns version-space-test (:use [clojure.test]))

(load "src/version_space")

(deftest test-generalize
  (testing "should return an empty list if passed an empty hypothesis"
    (is
      (=
        (version-space/generalize '() '("a" "b"))
        '()
      )
    )
  )
  
  (testing "generalizes the two passed hypotheses"
    (is
      (=
        (version-space/generalize '("a") '("a"))
        '("a")))
      
    (is
      (=
        (version-space/generalize '(:_) '("a"))
        '("a")))

    (is
      (=
        (version-space/generalize '("a") '(:_))
        '("a")))
    
    (is
      (=
        (version-space/generalize '("a") '("b"))
        '(:*)))
  )
)

(deftest test-more-general?
  (testing "should return true if the first list is equal to the second"
    (is (version-space/more-general? '("a") '("a")))
    (is (not (version-space/more-general? '("a") '("b"))))
  )
  
  (testing "should return true if the first list is more general than the second"
    (is (version-space/more-general? '("a" :* "c") '("a" "b" "c")))
    (is (not (version-space/more-general? '("a" :* "c") '("a" "b" "d"))))
  )
)

(deftest test-includes?
  (testing "should return true if both parameters are equal"
    (is (version-space/includes? "a" "a"))
    (is (not (version-space/includes? "a" "b")))
  )

  (testing "should return true if the first parameter is more general"
    (is (version-space/includes? :* "a"))
    (is (version-space/includes? :* "b"))
  )
)

(deftest test-get-potential-positions
  (testing "should return a list of positions that can be specialized"
    (is 
      (=
        (version-space/get-potential-positions '("rund" "blau") '("rund" "gelb") '("rund" "blau"))
        '()
      )
    )
    
    (is 
      (=
        (version-space/get-potential-positions '("rund" :*) '("rund" "gelb") '("rund" "blau"))
        '(1)
      )
    )
  )
)

(deftest test-specialize
  (testing "should return a list of specialized hypotheses"
    (is
      (=
        (version-space/specialize '("rund" :*) '("rund" "gelb") '("rund" "blau"))
        '(("rund" "blau"))
      )
    )

    (is
      (=
        (version-space/specialize '(:* :*) '("eckig" "blau") '("rund" "blau"))
        '(("rund" :*))
      )
    )

    (is
      (=
        (version-space/specialize '(:* :* :*) '("eckig" "blau" "groß") '("rund" "blau" "klein"))
        '(("rund" :* :*) (:* :* "klein"))
      )
    )
  )
)

(deftest test-more-complicated
  (testing "blaaaa"
    (let [vs '(((:* :* :*)) ((:_ :_ :_)))]
      (is
        (=
          (version-space/positive-example vs '("Sängerin" "Jazz" "20er-50er"))
          '(((:* :* :*)) (("Sängerin" "Jazz" "20er-50er")))
        )
      )
      
      (let [vs (version-space/positive-example vs '("Sängerin" "Jazz" "20er-50er"))]
        (is
          (=
            (version-space/negative-example vs '("Gruppe" "Pop" "70er"))
            '((("Sängerin" :* :*) (:* "Jazz" :*) (:* :* "20er-50er")) (("Sängerin" "Jazz" "20er-50er")))
          )
        )
      )
    )
  )
)



(run-tests)