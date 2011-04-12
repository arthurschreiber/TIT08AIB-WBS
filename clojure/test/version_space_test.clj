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

(deftest test-gen-list
  (testing "should return a list containing the passed `item` `size` times"
    (is
      (=
        (version-space/gen-list :* 3)
        '(:* :* :*)
      )
    )
  )
)

(deftest test-gen-version-space
  (testing "should create a new version space of the specified `size`"
    (is
      (=
        (version-space/gen-version-space 3)
        '(((:* :* :*)) ((:_ :_ :_)))
      )
    )
  )
)

(deftest test-more-complicated-saengerin
  (testing "should be correctly building a version space for the 'Sänger/Sängerin' example"
    (let [vs '(((:* :* :*)) ((:_ :_ :_)))]
      (let [vs (version-space/positive-example vs '("Sängerin" "Jazz" "20er-50er"))]
        (is
          (=
            vs
            '(((:* :* :*)) (("Sängerin" "Jazz" "20er-50er")))
          )
        )

        (let [vs (version-space/negative-example vs '("Gruppe" "Pop" "70er"))]
          (is
            (=
              vs
              '((("Sängerin" :* :*) (:* "Jazz" :*) (:* :* "20er-50er")) (("Sängerin" "Jazz" "20er-50er")))
            )
          )
          
          (let [vs (version-space/negative-example vs '("Gruppe" "Pop" "80er"))]
            (is
              (=
                vs
                '((("Sängerin" :* :*) (:* "Jazz" :*) (:* :* "20er-50er")) (("Sängerin" "Jazz" "20er-50er")))
              )
            )

            (let [vs (version-space/negative-example vs '("Sänger" "Jazz" "20er-50er"))]
              (is
                (=
                  vs
                  '((("Sängerin" :* :*)) (("Sängerin" "Jazz" "20er-50er")))
                )
              )

              (let [vs (version-space/positive-example vs '("Sängerin" "Jazz" "50er-60er"))]
                (is
                  (=
                    vs
                    '((("Sängerin" :* :*)) (("Sängerin" "Jazz" :*)))
                  )
                )

                (let [vs (version-space/negative-example vs '("Orchester" "Klassik" "vor 1920"))]
                  (is
                    (=
                      vs
                      '((("Sängerin" :* :*)) (("Sängerin" "Jazz" :*)))
                    )
                  )
                  
                  (let [vs (version-space/positive-example vs '("Sängerin" "Jazz" "70er"))]
                    (is
                      (=
                        vs
                        '((("Sängerin" :* :*)) (("Sängerin" "Jazz" :*)))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(deftest test-more-complicated-autokauf
  (testing "should be correctly building a version space for the 'Autokauf'"
    (let [vs '(((:* :* :* :* :* :* :* :* :* :* :*)) ((:_ :_ :_ :_ :_ :_ :_ :_ :_ :_ :_)))]
      (let [vs (version-space/positive-example vs '("neu" "VW" "90-120" "< 2 l" "< 180" "Diesel" "< 6 l" "Minivan" "8" "silber/grau" "< 25000"))]
        (is
          (=
            vs
            '(
              ((:* :* :* :* :* :* :* :* :* :* :*))
              (("neu" "VW" "90-120" "< 2 l" "< 180" "Diesel" "< 6 l" "Minivan" "8" "silber/grau" "< 25000"))
            )
          )
        )
        
        (let [vs (version-space/positive-example vs '("< 2 Jahre" "VW" "90-120" "< 2 l" "< 180" "Diesel" "< 6 l" "Minivan" "8" "grün" "< 20000"))]
          (is
            (=
              vs
              '(
                ((:* :* :* :* :* :* :* :* :* :* :*))
                ((:* "VW" "90-120" "< 2 l" "< 180" "Diesel" "< 6 l" "Minivan" "8" :* :*))
              )
            )
          )
          
          (let [vs (version-space/negative-example vs '("2-5 Jahre" "Peugeot" "75-90" "< 2 l" "< 180" "Super" "< 8 l" "kompakt" "5" "silber/grau" "< 7500"))]
            (is
              (= vs 
                '(
                  (
                    (:* "VW" :* :* :* :* :* :* :* :* :*)
                    (:* :* "90-120" :* :* :* :* :* :* :* :*)
                    (:* :* :* :* :* "Diesel" :* :* :* :* :*)
                    (:* :* :* :* :* :* "< 6 l" :* :* :* :*)
                    (:* :* :* :* :* :* :* "Minivan" :* :* :*)
                    (:* :* :* :* :* :* :* :* "8" :* :*)
                  )
                  ((:* "VW" "90-120" "< 2 l" "< 180" "Diesel" "< 6 l" "Minivan" "8" :* :*))
                )
              )
            )
          )
        )
      )
    )
  )
)



(run-tests)