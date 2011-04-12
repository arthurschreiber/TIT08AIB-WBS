(ns version-space)

(defn generalize [hyp1 hyp2]
  (map
    (fn [a b]
      (cond
        (= a :_) b
        (= b :_) a
        (= a b) a
        true :*
      )
    )
    hyp1 hyp2
  )
)

(defn includes? [a b]
  (or (= a b) (= a :*))
)

(defn more-general? [a b]
  (every? true? (map includes? a b))
)

(defn get-potential-positions [g neg s]
  (keep-indexed
    (fn [index potential] (if (true? potential) index) )
    (map
      (fn [g_item neg_item s_item]
        (and
          (= g_item :*)
          (not (= s_item neg_item))
        )
      )
      g neg s
    )
  )
)

(defn specialize-at-position [g s pos]
  (if (= pos 0)
    (cons (first s) (rest g))
    (cons (first g) (specialize-at-position (rest g) (rest s) (- pos 1)))
  )
)

(defn specialize [g neg s]
  (map 
    (fn [pos] (specialize-at-position g s pos) )
    (get-potential-positions g neg s)
  )
)

(defn positive-example [vs example]
  (list
    (filter
      (fn [g]
        (more-general? g example)
      )
      (first vs)
    )
    
    (filter
      (fn [s]
        (not-any?
          (fn [g]
            (more-general? s g)
          )
          (first vs)
        )
      )
      (map
        (fn [s]
          (if (not (more-general? s example)) (generalize s example) s)
        )
        (second vs)
      )
    )
  )
)

(defn negative-example [vs example]
  (list
    (reduce
      (fn [acc g]
        (if
          (not (more-general? g example))
          (concat acc (list g))
          (concat acc
            (reduce
              (fn [acc new_g]
                (if
                  (and
                    (not (more-general? new_g example))
                    (every? (fn [pe] more-general? new_g pe) (second vs))
                    (not-any?
                      (fn [other_g]
                        (and
                          (not (= g other_g))
                          (more-general? other_g new_g)
                        )
                      )
                      (first vs)
                    )
                  )
                  (concat acc (list new_g))
                  acc
                )
              )
             '()
              (specialize g example (first (second vs)))
            )
          )
        )
      )
      '() (first vs)
    )
    
    (filter
      (fn [s]
        (not (more-general? s example))
      )
      (second vs)
    )
  )
)