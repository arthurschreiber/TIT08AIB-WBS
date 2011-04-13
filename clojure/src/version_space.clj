(ns version-space)

;; Erstellt eine neue Hypothese, die genereller als die beiden
;; übergebenen Hypothesen ist.
(defn generalize [hyp1 hyp2]
  (map (fn [a b]
    (cond
      (= a :_) b
      (= b :_) a
      (= a b) a
      true :*
    )
  ) hyp1 hyp2)
)

(defn includes? [a b]
  (or (= a b) (= a :*))
)

(defn more-general? [a b]
  (every? true? (map includes? a b))
)

;; Gibt eine Liste von Positionen zurück, an denen `g`
;; spezialisiert werden kann.
(defn get-potential-positions [g neg s]
  (keep-indexed (fn [index potential]
    (if (true? potential) index)
  ) (map (fn [g_item neg_item s_item]
      (and
        (= g_item :*)
        (not (= s_item neg_item))
      )
    ) g neg s)
  )
)

;; Spezialisiert `g` an der Position `pos`
(defn specialize-at-position [g s pos]
  (if (= pos 0)
    (cons (first s) (rest g))
    (cons (first g) (specialize-at-position (rest g) (rest s) (- pos 1)))
  )
)

;; Erstellt eine Liste von Spezialisierungen der Hypothese `g`
;; an Hand des Negativ-Beispieles `neg` und der speziellen Hypothese `s`
(defn specialize [g neg s]
  (map 
    (fn [pos] (specialize-at-position g s pos) )
    (get-potential-positions g neg s)
  )
)

;; Der Versionsraum `vs` wird mit Hilfe des Positiv-Beispiels
;; `example` verfeinert
(defn positive-example [vs example]
  (list
    ;; Lösche alle Elemente aus G, die das Positiv-Beispiel nicht enthalten
    (filter (fn [g]
      (more-general? g example)
    ) (first vs))
    
    ;; Streiche alle Elemente von S, die allgemeiner sind als ein Element von G
    (filter (fn [s]
      (not-any? (fn [g]
        (more-general? s g)
      ) (first vs))
    ) (map (fn [s]
      ;; Ersetze alle Elemente aus S, die das Positiv-Beispiel nicht
      ;; enthalten durch eine Verallgemeinerung
      (if
        (not (more-general? s example))
        (generalize s example)
        s
      )
    ) (second vs)) )
  )
)

;; Der Versionsraum `vs` wird mit dem übergebenen Negativ-Beispiel
;; `example` verfeinert.
(defn negative-example [vs example]
  (list
    (reduce (fn [acc g]
      (if
        (not (more-general? g example))
        (concat acc (list g))
        (concat acc
          ;; Generiere alle Spezialisierungen und sammele diejenigen, die alle
          ;; Kriterien erfüllen.
          (reduce (fn [acc new_g]
            (if
              (and
                ;; Die Spezialisierung darf das negative Beispiel nicht enthalten
                (not (more-general? new_g example))
                ;; Alle bisher vorgelegten Beispiele müssen enthalten sein
                ;; NOTE: Das stimmt hier eigentlich nicht _direkt_. Wir testn hier
                ;;       eigentlich gegen die Elemente von S. Da S aber in unserem
                ;;       Fall dem positiven Beispielen entspricht, passt das soweit.
                (every? (fn [pe]
                  more-general? new_g pe
                ) (second vs))
                ;; Kein Element von G darf die neue Spezialisierung enthalten,
                ;; da sie ja sonst überflüssig ist.
                (not-any? (fn [other_g]
                  (and
                    (not (= g other_g))
                    (more-general? other_g new_g)
                  )
                ) (first vs) )
              )
              ;; Die neue Spezialisierung erfüllt alle Kriterien
              ;; -> wird zum Akkumulator hinzugefügt.
              (concat acc (list new_g))
              ;; Die neue Spezialisierung erfüllt die Kriterien nicht
              ;; -> wird nicht zum Akkumulator hinzugefügt.
              acc
            )
          ) '() (specialize g example (first (second vs))) )
        )
      )
    ) '() (first vs) )
    
    ;; Alle Elemente aus S entfernen, die dem negativen Beispiel entsprechen
    (filter
      (fn [s]
        (not (more-general? s example))
      )
      (second vs)
    )
  )
)

;; Generiert eine neue Liste, in der `item` `size` mal vorkommt.
(defn gen-list [item size]
  (if
    (<= size 0)
    '()
    (cons
      item
      (gen-list item (- size 1))
    )
  )
)

;; Generiert einen neuen, leeren Versionsraum
(defn gen-version-space [size]
  (list
    (list (gen-list :* size))
    (list (gen-list :_ size))
  )
)