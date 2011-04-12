(ns example
  (:use [clojure.string :only (split)])
  (:use [clojure.java.io :only (reader)]))
(load "src/aq_algorithm")

(let [ examples (map (fn [line] (split line #";")) (rest (line-seq (reader "example/input.csv"))))
       vs (version-space/gen-version-space (count (first examples)))
       groups (group-by (fn [example] (last example)) examples) ]

  (doseq [key (keys groups)]
    (let [ positives (aq-algorithm/get-positive-examples groups key)
           negatives (aq-algorithm/get-negative-examples groups key) ]
    
      (println "K für" key)
      (doseq [k (aq-algorithm/gen-concept-space positives negatives)] (println k))
    )
  )
)