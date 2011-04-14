       
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

(defun get-potential-positions (g neg s pos poslist)  
  (cond ((null g) poslist)
        ((equal (car g) "*")
         (cond ((equal (car neg) (car s)) 
                (get-potential-positions (cdr g) (cdr neg) (cdr s) (+ pos 1) poslist))
               (T (get-potential-positions (cdr g) (cdr neg) (cdr s) (+ pos 1) (cons pos poslist)))))
        (T (get-potential-positions (cdr g) (cdr neg) (cdr s) (+ pos 1) poslist))
))


(defun specialize-first-attribute (g s)
  (cond ((equal (car g) "*") (cons (car s) (cdr g)))
        (T nil)))
         
(defun specialize-position (g neg s pos)
  (cond ((= pos 0) (specialize-first-attribute g s))
        ((> pos 0) (cons (car g)
                         (specialize-position (cdr g) (cdr neg) (cdr s) (- pos 1))))))

 
(defun specialize (g neg s)
  (let ((pos (get-potential-positions g neg s 0 nil)))
    (mapcar #'(lambda (x) (specialize-position g neg s x)) pos)))


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