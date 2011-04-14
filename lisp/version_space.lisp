       
; Version Space 
; -------------

; Examples :   
;
;  generalize - a VS generalization for 2 hypotheses 
;  specialize - the set "G" is specialized, in this case: one element of g

(defun generalize (hyp1 hyp2)
  (mapcar #'(lambda (a b)
    (cond
      ((equal a "_") b)
      ((equal b "_") a)
      ((equal a b) a)
      (T "*")
    )
  ) hyp1 hyp2)
)

; --- some helpers ---

(defun includes? (x y)
  (or (equal x y) (equal x "*"))
)

(defun more-general? (a b)
  (every 'includes? a b)
)

; --- specialize ---

(defun position-can-be-specialized? (g_item neg_item s_item)
  (and (equal g_item "*") (not (equal s_item neg_item)))
)

(defun get-potential-positions (g neg s)
  ;; Holy Crap is this ugly ...
  (loop for g_item in g
    for neg_item in neg
    for s_item in s
    for i from 0
    if (position-can-be-specialized? g_item neg_item s_item)
    collect i
  )
)

;; Spezialisiert `g` an der Position `pos`
(defun specialize-position (g s pos)
  (if (= pos 0)
    (cons (first s) (rest g))
    (cons (first g) (specialize-position (rest g) (rest s) (- pos 1)))
  )
)

(defun specialize (g neg s)
  (mapcar #'(lambda (pos)
    (specialize-position g s pos)
  ) (get-potential-positions g neg s))
)

;; Generiert eine neue Liste, in der `item` `size` mal vorkommt.
(defun gen-list (item size)
  (if (<= size 0)
    '()
    (cons
      item
      (gen-list item (- size 1))
    )
  )
)

;; Generiert einen neuen, leeren Versionsraum
(defun gen-version-space (size)
  (list
    (list (gen-list "*" size))
    (list (gen-list "_" size))
  )
)

;; Der Versionsraum `vs` wird mit Hilfe des Positiv-Beispiels
;; `example` verfeinert
(defun positive-example (vs example)
  (let ((VS_G (first vs)) (VS_S (second vs)))
    ;; Ein neuer Versionsraum...
    (list
      ;; VS_G ohne diejenigen Hypothesen,
      ;; die das Positiv-Beispiel nicht enthalten
      (remove-if-not #'(lambda (g) (more-general? g example)) VS_G)
    
      ;; Streiche alle Elemente der neuen VS_S-Menge,
      ;; die allgemeiner sind als ein Element von VS_G
      (remove-if-not
        #'(lambda (s) (notany #'(lambda (g) (more-general? s g)) VS_G))
        (mapcar #'(lambda (s)
          ;; Ersetze alle Elemente aus S, die das Positiv-Beispiel nicht
          ;; enthalten durch eine Verallgemeinerung
          (if (not (more-general? s example))
            (generalize s example)
            s
          )
        ) VS_S)
      )
    )
  )
)

;; Der Versionsraum `vs` wird mit dem übergebenen Negativ-Beispiel
;; `example` verfeinert.
(defun negative-example (vs example)
  (let ((VS_G (first vs)) (VS_S (second vs)))
    ;; Ein neuer Versionsraum ...
    (list
      (reduce #'(lambda (acc g)
        (if
          (not (more-general? g example))
          (append acc (list g))
          (append acc
            ;; Generiere alle Spezialisierungen und sammele diejenigen, die alle
            ;; Kriterien erfüllen.
            (reduce #'(lambda (acc new_g)
              (if
                (and
                  ;; Die Spezialisierung darf das negative Beispiel nicht enthalten
                  (not (more-general? new_g example))
                  ;; Alle bisher vorgelegten Beispiele müssen enthalten sein
                  ;; NOTE: Das stimmt hier eigentlich nicht _direkt_. Wir testen hier
                  ;;       eigentlich gegen die Elemente von VS_S. Da VS_S aber in unserem
                  ;;       Fall dem positiven Beispielen entspricht, passt das soweit.
                  (every #'(lambda (pe)
                    (more-general? new_g pe)
                  ) VS_S)
                  ;; Kein Element von VS_G darf die neue Spezialisierung enthalten,
                  ;; da sie ja sonst überflüssig ist.
                  (notany #'(lambda (other_g)
                    (and
                      (not (equal g other_g))
                      (more-general? other_g new_g)
                    )
                  ) VS_G)
                )
                ;; Die neue Spezialisierung erfüllt alle Kriterien
                ;; -> wird zum Akkumulator hinzugefügt.
                (append acc (list new_g))
                ;; Die neue Spezialisierung erfüllt die Kriterien nicht
                ;; -> wird nicht zum Akkumulator hinzugefügt.
                acc
              )
            ) (specialize g example (first VS_S)) :initial-value '() )
          )
        )
      ) VS_G :initial-value '() )
    
      ;; Alle Elemente aus S entfernen, die dem negativen Beispiel entsprechen
      (remove-if-not #'(lambda (s) (not (more-general? s example))) VS_S)
    )  
  )
)

; --- how to read an exampleset from a file:

; --- helper / makes the result more readable in case there are "nil" entries

(defun prune (x)
  (cond ((null x) nil)
        ((null (car x)) (prune (cdr x)))
        (T (cons (car x) (prune (cdr x))))))

(defun read-exampleset (file)
   (LET ((STREAM (OPEN file :DIRECTION :INPUT)))
     (DO ((expression NIL (READ STREAM NIL STREAM)) 
          (xprlist nil (cons expression xprlist)))
         ((EQ expression STREAM) (progn (CLOSE STREAM) (reverse (prune xprlist)))))))

; --- example:

; Generate a file in which you may write the lecture example:
; 
; ("saengerin" "jazz" "20er-50er")
; ("saenger" "jazz" "20er-50er")
;
; (read-exampleset "yourfilename") should then return the list
;
; (("saengerin" "jazz" "20er-50er")("saenger" "jazz" "20er-50er"))
;