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

(defun sum2(lst)
	(cond
		((null lst) 0)
		(T (+ (first lst) (sum (rest lst))))
	)
)

(defun klausurnote(prüfungsnote &rest übungsnoten)
	(let ((factor (min (/ (length übungsnoten) 10.0) 0.5)))
	(+ (* factor (notendurchschnitt übungsnoten)) (* (- 1.0 factor) prüfungsnote))))


(defun mobilep (gehänge)
	(cond
		((and (listp gehänge) (equal 3 (length gehänge)))
       (let ((left (mobilep (second gehänge)))
           (right (mobilep (third gehänge))))
   		(if (and (equal left right) left)
				(+ (first gehänge) left right) nil)))
		((numberp gehänge) gehänge)))

(defun mobilep1 (gehänge)
	(cond
		((numberp gehänge) gehänge)
		((and (listp gehänge) (numberp (first gehänge)) (equal 3 (length gehänge))
			(equal (mobilep1 (second gehänge)) (mobilep1 (third gehänge))))
			(+ (first gehänge) (mobilep1 (second gehänge)) (mobilep1 (third gehänge))))
   ))

(setf m '(1 (1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))))(1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))))
