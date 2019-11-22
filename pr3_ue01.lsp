(defun endnote-safe (prüfungsnote &optional vornote)
		(cond ((< vornote prüfungsnote)(/ (+ vornote prüfungsnote) 2.0))
		(T prüfungsnote)) prüfungsnote))

(defun endnote2 (prüfungsnote &optional vornote1 vornote2)
	(cond
		(if (AND (< vornote1 prüfungsnote) (< vornote2 prüfungsnote)) (/ (+ prüfungsnote vornote1 vornote2) 3.0)))(prüfungsnote)
   	(T prüfungsnote))

(defun notendurchschnitt (listevonnoten &optional bonus)
	(if (AND listevonnoten (listp listevonnoten))
				(max (- (/ (sum listevonnoten) (length listevonnoten)) (OR bonus 0.0)) 0.7)
  )
)

(defun sum (lst)
	(cond
		((AND lst (listp lst)) (+ (first lst) (sum (rest lst))))
		(T 0)))

(defun klausurnote(prüfungsnote &rest übungsnoten)
	(let ((factor (min (/ (length übungsnoten) 10.0) 0.5)))
	(+ (* factor (notendurchschnitt übungsnoten)) (* (- 1.0 factor) prüfungsnote))))

(defun mobilep (gehänge)
  (cond
    ((numberp gehänge) gehänge)
    ((listp gehänge)
     (let ((left (mobilep (second gehänge)))
           (right (mobilep (third gehänge))))
       (if (and left (equal left right))
         (+ (first gehänge) left right))))))

;test mobile
(setf m '(1 (1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))))(1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))))))
;not a mobile
(setf nm '(1 (1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))))(1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 3 3)(2 2 2)) (1 (1 2 2)(1 2 2))))))))
