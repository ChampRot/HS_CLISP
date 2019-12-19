;Testat 2 - PR3
; CLISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 1 a)

(defun berechnen (l)
  (mapcar
    #'(lambda (sublst)(append sublst
        (list '=
              (apply (second sublst) (list (first sublst) (third sublst)))
        ))
      )
    l
  )
)

(format t "Aufgabe 1 a)~%")
(write (berechnen '((4 + 5) (2 * 9) (3 - 9) (6 / 2) (5 + 7))))
(format t "~%")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 1 b)


; Rekursiv
(defun vzs-r (lst)
  (and (first lst)
    (append (list (get-first lst)) (vzs-r (get-rest lst)))
  )
)

(defun get-first (lst)
  (and lst
    (append
      (list (first (first lst))) (get-first (rest lst))
    )
  )
)

(defun get-rest (lst)
  (and lst
    (append
      (list (rest (first lst))) (get-rest (rest lst))
    )
  )
)

(format t "Aufgabe 1 b)~%")

(format t "rekursiv~%")
(write (vzs-r '((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5))))
(format t "~%")



; Mit do-Schleife
(defun vzs-do (lst)
  (let ((res-lst nil)(tmp nil))
    (do ((i 0 (1+ i))) ((= i (length (first lst))))
      (do ((j 0 (1+ j))) ((= j (length lst)))
        (setq tmp (append tmp (list (nth i (nth j lst)))))
      )
      (setq res-lst (append res-lst (list tmp)))(setq tmp nil)
    )
  res-lst
  )
)

(format t "do::~%")
(write (vzs-do '((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5))))
(format t "~%")

; mit mapcar
(defun vzs-mapcar (lst)
  (let ((res-lst nil))
    (mapcar #'(lambda (e)
      (setq res-lst (append res-lst (list (mapcar #'first lst))))
      (setq lst (mapcar #'rest lst)))
    (first lst)
    )
  res-lst
  )
)

(format t "mapcar::~%")
(write (vzs-mapcar '((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5))))
(format t "~%")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 1 c)

(defun arith-eval-1 (lst)
  (cond
    ((and (get-p #'(lambda (e)(not (numberp e))) (rest lst))
      (< 1 (length (get-p #'numberp lst))))
      (append
        (list
          (first lst)
          (apply (first lst)
            (get-p #'numberp lst)
          )
        )
        (get-p #'(lambda (e)(not (numberp e))) (rest lst))
      )
    )
    ((>= 1 (length (get-p #'numberp lst))) lst)
    (T (apply (first lst) (rest lst)))
  )
)

(defun get-p (p lst)
  (mapcan #'(lambda (e) (if (apply p (cons e nil)) (list e))) lst)
)

(format t "Aufgabe 1 c)~%")
(write (arith-eval-1 '(+ 5 4 a 9 7)))
(format t "~%")
(write (arith-eval-1 '(+ 5 (* a 3) 9 (- 8 2))))
(format t "~%")
(write (arith-eval-1 '(- 5 4)))
(format t "~%")
(write (arith-eval-1 '(> 5 4)))
(format t "~%")
(write (arith-eval-1 '(/ x 4)))
(format t "~%")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 1 d)

(defun arith-eval-2 (lst)
  (arith-eval-1
    (mapcar
      #'(lambda (e)
          (cond
            ((listp e) (arith-eval-2 e))
            (T e)
          )
        )
      lst
    )
  )
)


(format t "Aufgabe 1 d)~%")
(write (arith-eval-2 '(+ 5 4 (* a 3) 9 (- 8 2))))
(format t "~%")
(write (arith-eval-2 '(+ 5 4 (* 3 (- 8 5)) a 9 (+ 2 1))))
(format t "~%")
(write (arith-eval-2 '(+ 5 4 (* (+ 2 3 5) 3) 1 (- 8 2))))
(format t "~%")
(write (arith-eval-2 '(= (* 3 5) (* a 3))))
(format t "~%")
(write (arith-eval-2 '(< (* 2 3) (- 9 2))))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 1 e)

(defun sub-liste-von-listen (lst &rest more)
  (mapcar #'(lambda (sub-lst)
    (mapcar #'(lambda (e c)
      (- e (/ (* c (first sub-lst)) (first lst)))) sub-lst lst))
  more)
)

(format t "Aufgabe 1 e)~%")
(write (sub-liste-von-listen '(2 3 4 5) '(4 7 5 11) '(-2 -5 8 0)))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 1 f)

(defun bilde-geordnete-Unterlisten (fkt lst)
  (let (res)
    (cond
      ((< (length lst) 2)
        (list lst))
      ((apply fkt (list (first lst) (second lst)))
        (setq res (bilde-geordnete-Unterlisten fkt (rest lst)))
        (cons (cons (first lst) (first res)) (rest res)))
      (T (cons (list (first lst))
        (bilde-geordnete-Unterlisten fkt (rest lst))))
    )
  )
)


(format t "Aufgabe 1 f)~%")
(write (bilde-geordnete-Unterlisten '< '(2 5 9 3 34 44 45 45 56 3 3 5 9 0)))
(format t "~%")
(write (bilde-geordnete-Unterlisten
  (lambda (x y) (equal (first x) (first y)))
  '((a 2) (b 5) (b 9) (b 3) (c 34) (c 44))))
(format t "~%")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 2 a)

(defun alist-keys (lst)
  (and
    lst
    (append
      (list (first (first lst)))
      (alist-keys (rest lst))
    )
  )
)

(defun mapcar-alist-keys (lst)
  (mapcar #'(lambda (sub-lst) (first sub-lst)) lst)
)


(format t "Aufgabe 2 a)~%")
(write (alist-keys '((d 3) (a 1) (b 7) (e 0))))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 2 b)

(defun listenop (op lst1 lst2)
  (apply op (list lst1 lst2))
)

(format t "Aufgabe 2 b)~%")
(write (listenop 'list '(3 4) '((c d) (e f))))
(format t "~%")
(write (listenop 'append '(3 4) '((c d) (e f))))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 2 c)

(defun deep-member (expr liste)
  (cond
    ((null liste) nil)
    ((equal (first liste) expr) T)
    ((listp (first liste))
      (or
        (deep-member expr (first liste))
        (deep-member expr (rest liste)))
    )
    (T (deep-member expr (rest liste)))
  )
)

(format t "Aufgabe 2 c)~%")
(write (deep-member 'd '(a b (c a d) e)))
(format t "~%")
(write (deep-member '(c a d) '(a (b (c a d)) e)))
(format t "~%")
(write (deep-member 'f '(a b c a d e)))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 2 d)

(defun verteilen (lst)
  (append
    (list (mapcan #'(lambda (e) (if (not (numberp e)) (list e))) lst))
    (list (mapcan #'(lambda (e) (if (numberp e) (list e))) lst))
  )
)

(format t "Aufgabe 2 d)~%")
(write (verteilen '(a 9 8 b e 2 k)))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 2 e)

(defun div-first (lst)
  (cond
    ((= (first lst) 0) '(div_durch_null))
    (T (append (list 1) (div-first-help lst)))
  )
)

(defun div-first-help (lst)
  (cond
    ((<= 2 (length lst))
      (append
        (list
          (/ (second lst) (first lst) ))
        (div-first-help
          (append
            (list (first lst))
            (rest  (rest lst))
          )
        )
      )
    )
  )
)

(format t "Aufgabe 2 e)~%")
(write (div-first '(3 9 15 12 30)))
(format t "~%")
(write (div-first '(0 9 15 12 30)))
(format t "~%")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 2 f)

(defun bilde-geordnete-Unterlisten (lst)
  (let (res)
    (cond
      ((< (length lst) 2)
        (list lst))
      ((< (first lst) (second lst))
        (setq res (bilde-geordnete-Unterlisten (rest lst)))
        (cons (cons (first lst) (first res)) (rest res)))
      (T (cons (list (first lst))
        (bilde-geordnete-Unterlisten (rest lst))))
    )
  )
)

(format t "Aufgabe 2 f)~%")
(write (bilde-geordnete-Unterlisten '(2 5 9 3 34 44 45 45 56 3 3 5 9 0)))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aufgabe 2 g)

(defun vereinfachen (lst)
  (cond
    ((and (equal (first lst) '*) (member 0 lst)) 0)
    (T (entferne-neutrale-elemente
      (mapcan
        #'(lambda (e) (if (listp e) (list (vereinfachen e)) (list e)))
        lst)))
  )
)

(defun entferne-neutrale-elemente (lst)
  (cond
    ((equal (first lst) '+)
      (mapcan #'(lambda (e)(if (equal e 0) nil (list e))) lst))
    ((equal (first lst) '*)
      (mapcan #'(lambda (e)(if (equal e 1) nil (list e))) lst))
  )
)

(format t "Aufgabe 2 g)~%")
(write (vereinfachen '(+ a (* (+ b (* 0 d) c) 1 3) 0)))
(format t "~%")
