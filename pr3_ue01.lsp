;Aufgabe 1
(defun endnote (prüfungsnote &optional vornote)
	(cond
  	((and vornote (< vornote prüfungsnote))
     	(/ (+ vornote prüfungsnote) 2.0)
  	)
		(T prüfungsnote)
  )
)

(defun endnote2 (prüfungsnote &optional vornote1 vornote2)
	(cond
   	((AND vornote1 vornote2 (< vornote1 prüfungsnote) (< vornote2 prüfungsnote))
   		(/ (+ prüfungsnote vornote1 vornote2) 3.0)
    )
   	(T (endnote prüfungsnote (or vornote1 vornote2)))
  )
)

(defun notendurchschnitt (listevonnoten &optional bonus)
	(if (listp listevonnoten)
		(max
 			(- (/ (sum listevonnoten) (length listevonnoten)) (OR bonus 0.0))
    	0.7
    )
  )
)

(defun sum (lst)
	(cond
		((and lst (listp lst)) (+ (first lst) (sum (rest lst))))
		(T 0)
  )
)

(defun klausurnote(prüfungsnote &rest übungsnoten)
	(if übungsnoten
  	(let ((factor (min (/ (length übungsnoten) 10.0) 0.5)))
			(+ (* factor (notendurchschnitt übungsnoten))
	    	 (* (- 1.0 factor) prüfungsnote)
	  	)
	  )
   	prüfungsnote
  )
)

;Aufgabe 2
(defun mobilep (gehänge)
  (cond
    ((numberp gehänge) gehänge)
    ((listp gehänge)
     	(let ((left (mobilep (second gehänge)))
	         	(right (mobilep (third gehänge)))
	         )
	     (if (and left (equal left right))
	         (+ (first gehänge) left right))
     	)
    )
  )
)

;test mobile
(setf m '(1 (1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))))(1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))))))
;not a mobile
(setf nm '(1 (1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))))(1 (1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1)))))(1 (1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))))(1 (1 (1 (1 1 1)(1 1 1)) (1 (1 1 1)(1 1 1))) (1 (1 (1 3 3)(2 2 2)) (1 (1 2 2)(1 2 2))))))))
