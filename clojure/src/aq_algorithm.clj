(ns aq-algorithm
  (:require clojure.contrib.greatest-least)
  (:use [clojure.contrib.greatest-least :only (least-by)])
)

(load "src/version_space")

(defn normalize-example [example]
  (rest (butlast example))
)

(defn get-positive-examples [groups key]
  (map normalize-example (get groups key))
)

(defn get-negative-examples [groups key]
  (map normalize-example (reduce concat '() (vals (dissoc groups key))))
)

(defn gen-star [pos_example negatives]
  (first
    (reduce (fn [vs neg_example]
      (version-space/negative-example vs neg_example)
    ) (version-space/positive-example
        (version-space/gen-version-space (count pos_example))
        pos_example
    ) negatives)
  )
)

(defn select-best-generalization [gen_star]
  ; (first gen_star)
  (apply least-by (fn [g]
    (reduce (fn [acc item]
      (if (= item :*) (+ acc 1) acc)
    ) 0 g)
  ) gen_star)
)

(defn gen-best-generalization [pos_example negatives]
  (select-best-generalization (gen-star pos_example negatives))
)

(defn gen-concept-space-helper [positives negatives k]
  (if
    (empty? positives) k
    (let [ g (gen-best-generalization (first positives) negatives) ]
      (gen-concept-space-helper
        (remove (fn [pos_example]
          (version-space/more-general? g pos_example)
        ) positives)
      negatives (cons g k))
    )
  )
)

(defn gen-concept-space [positives negatives]
  (gen-concept-space-helper positives negatives '())
)