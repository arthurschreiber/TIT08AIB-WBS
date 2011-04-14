       
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