(ns example
  (:use [clojure.java.io :only (reader)])
  (:use [clojure.string :only (split)])
)

(load "src/version_space")


(defn get-positive-examples [groups key]
  (map (fn [example]
    (rest (butlast example))
  ) (get groups key))
)

(defn get-negative-examples [groups key]
  (map (fn [example]
    (rest (butlast example))
  ) (reduce concat '() (vals (dissoc groups key))))
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

(defn gen-best-generalization [pos_example negatives]
  ; TODO "bessere" generalisierung wählen
  (first (gen-star pos_example negatives))
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

(let [ examples (map (fn [line] (split line #";")) (rest (line-seq (reader "input.csv"))))
       vs (version-space/gen-version-space (count (first examples)))
       groups (group-by (fn [example] (last example)) examples) ]

  (doseq [key (keys groups)]
    (let [ positives (get-positive-examples groups key)
           negatives (get-negative-examples groups key) ]
    
      (println "K für" key)
      (doseq [k (gen-concept-space positives negatives)] (println k))
    )
  )
)