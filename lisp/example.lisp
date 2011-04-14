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

(load "version_space.lisp")

(defun negative-examples (examples key)
  (remove-if #'(lambda (example)
    (equal (first (last example)) key)
  ) examples)
)

(defun positive-examples (examples key)
  (remove-if-not #'(lambda (example)
    (equal (first (last example)) key)
  ) examples)
)

(defun gen-star (example examples)
  (first
    (reduce #'(lambda (vs neg_example)
      (negative-example vs (butlast neg_example))
    ) (negative-examples examples (last example)) :initial-value (positive-example (gen-version-space (- (length example) 1)) (butlast example)))
  )
)

(defun select-best-generalization (star positives negatives)
  ;; "Beste" Generalisierung - Erste Generalisierung
  ; (first star)

  ;; "Beste" Generalisierung - Letzte Generalisierung
  (first (last star))

  ;; "Beste" Generalisierung - Viele "*"
  ; (first (sort star #'> :key #'(lambda (g)
  ;   (reduce (lambda (acc item)
  ;     (if (equal item "*") (+ acc 1) acc)
  ;   ) g :initial-value 0)
  ; )))

  ;; "Beste" Generalisierung - Wenig "*"
  ; (first (sort star #'> :key #'(lambda (g)
  ;   (reduce (lambda (acc item)
  ;     (if (equal item "*") (+ acc 1) acc)
  ;   ) g :initial-value 0)
  ; )))

  ;; "Beste" Generalisierung - Möglichst viele Positive Beispiele abgedeckt
  ; (first (sort star #'> :key #'(lambda (g)
  ;   (reduce (lambda (acc item)
  ;     (if (more-general? g item) (+ acc 1) acc)
  ;   ) positives :initial-value 0)
  ; )))
)

(defun gen-best-generalization (pos_example negatives positives)
  (select-best-generalization
    (gen-star pos_example negatives)
    positives
    negatives
  )
)

(defun gen-concept-space-helper (positives negatives k)
  (if
    (equal 0 (length positives)) k
    (let ((g (gen-best-generalization (first positives) negatives positives)))
      (gen-concept-space-helper
        (remove-if #'(lambda (pos_example)
          (more-general? g pos_example)
        ) positives)
      negatives (cons g k))
    )
  )
)

(defun gen-concept-space (positives negatives)
  (gen-concept-space-helper positives negatives '())
)

(defun matches-for (example K)
  (reduce #'(lambda (result concept_space)
    (append result (reduce #'(lambda (matches generalization)
      (if (more-general? generalization (butlast example))
        (append matches (list (first concept_space)))
        matches
      )
    ) (second concept_space) :initial-value '()) )
  ) K :initial-value '())
)

(let
  ( (examples (read-exampleset "input.txt"))
    (test_examples (read-exampleset "test.txt"))
    (keys '("Buch_A" "Buch_B" "Buch_C"))
  )

  (let (
    (K (mapcar #'(lambda (key)
      (list key
        (gen-concept-space
          (positive-examples examples key)
          (negative-examples examples key)
        )
      )
    ) keys)))
  
    (let (
      (matches (mapcar #'(lambda (example)
        (list example (matches-for example K))
      ) test_examples)))

      (loop for match in matches do
        (format t "---------------------------------------~%")
        (format t "Datensatz: ~A~%" (butlast (first match)))
        (if (second match)
          (format t "Mögliche Buchwahl: ~A~%" (remove-duplicates (second match)))
          (format t "Leider keine Aussage möglich!~%")
        )
        (format t "Die korrekte Buchwahl: ~A" (last (first match)))
        (format t "~%")
        (format t "~%")
      )
      
      (format t "---------------------------------------~%")

      (format t "Anzahl an korrekten Vorhersagen ~A~%" (count-if #'(lambda (match)
        (and 
          (equal (list-length (second match)) 1)
          (find-if #'(lambda (x) (equal x (first (second match)))) (second match))
        )
      ) matches))
      
      (format t "Anzahl an nicht eindeutigen Vorhersagen ~A~%" (count-if #'(lambda (match)
        (and 
          (> (list-length (second match)) 1)
        )
      ) matches))

      (format t "Anzahl an falschen Vorhersagen ~A~%" (count-if #'(lambda (match)
        (not (find-if #'(lambda (x) (equal x (first (second match)))) (second match)))
      ) matches))
    )
  )
)
