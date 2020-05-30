(setf database 
    '((b1 shape brick) 
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

(defun match-element (s1 s2)
    (or (eq s2 '?) (eql s1 s2)))

(defun match-triple (t1 t2)
    (every #'match-element t1 t2))

(defun fetch (pattern)
    (remove-if-not #'(lambda (entry) (match-triple entry pattern)) 
        database))

(fetch '(b4 shape ?))
(fetch '(? shape brick))
(fetch '(b2 ? b3))
(fetch '(? color ?))
(fetch '(b4 ? ?))

(defun color (name)
    (list name 'color '?))

(defun supporters (block)
    (mapcar #'first (fetch (list '? 'supports block))))

(defun supp-cube (block)
    (let ((supps (supporters block))) 
        (find-if #'(lambda (x) 
            (and (member x supps) (fetch (list x 'shape 'cube)))) supps)))

(defun description (block)
    (rest (reduce #'(lambda (x y) (append x (rest y))) 
        (fetch (list block '? '?)))))

(defun fib (n)
    (cond ((or (= n 0) (= n 1)) 1) 
            (T (+ (fib (1- n))(fib (- n 2))))))

(defun sum-up-numerics (l)
    (cond ((null l) 0)
        ((numberp (first l)) (+ (first l) (sum-up-numerics (rest l))))
        (T (sum-up-numerics (rest l)))))

(defun my-remove (p l)
    (cond ((null l) nil)
        ((funcall p (first l)) (cons (first l) (my-remove p (rest l))))
        (T (my-remove p (rest l)))))

(defun my-intersection (s1 s2)
    (cond ((null s1) nil)
        ((member (first s1) s2) (cons (first s1) (my-intersection (rest s1) s2)))
        (T (my-intersection (rest s1) s2))))


(defun count-atoms (l)
    (cond ((null l) 1)
        ((atom (first l))(+ 1 (count-atoms (rest l))))
        (T (+ (count-atoms (first l))(count-atoms (rest l))))))

(defun count-cons (l)
    (cond ((atom l) 0)
        ((consp (first l)) (+ 1 (count-cons (first l)) (count-cons (rest l))))
        (t (+ 1 (count-cons (rest l))))))

(defun sum-tree (tree)
    (cond ((null tree) 0)
        ((numberp (first tree)) (+ (first tree) (sum-tree (rest tree))))
        ((consp (first tree)) (+ (sum-tree (first tree)) (sum-tree (rest tree))))
        (t (sum-tree (rest tree)))))

(defun my-subst (s u l)
    (cond ((null l) l)
        ((equal (first l) u) (cons s (my-subst s u (rest l))))
        (T (cons (first l) (my-subst s u (rest l))))))

(defun flatten (lst)
    (cond ((null lst) nil)
        ((consp (first lst))(append (flatten (first lst)) (flatten (rest lst))))
        (t (cons (first lst) (flatten (rest lst))))))

(defun tree-depth (lst)
    (cond ((or (null lst) (atom lst)) 0)
        ((consp (first lst)) (let ((max1 0)(max2 0))
            (setf max1 (1+ (tree-depth (first lst))))
            (setf max2 (1+ (tree-depth (rest lst))))
            (if (< max1 max2) max2 max1)))
        (T (+ 1 (tree-depth (rest lst))))))

(defun paren-depth (l) 
    (cond ((null l) 1)
        ((atom l) 0)
        ((consp (first l))(let (max1 max2)
            (setf max1 (1+ (paren-depth (first l))))
            (setf max2 (paren-depth (rest l)))
            (if (< max1 max2) max2 max1)))
        (T (paren-depth (rest l)))))

(defun count-up (n)
    (cond ((= n 0) nil)
        (T (append (count-up (1- n)) (list n)))))

(defun make-loaf (n)
    (if (= n 0) nil (append (make-loaf (1- n)) '(x))))

(defun bury (e n)
    (if (= n 0) e (cons (bury e (1- n)) nil)))

(defun pairings (l1 l2)
    (if (or (null l1) (null l2)) nil
    (cons (cons (first l1) (cons (first l2) nil)) 
        (pairings (rest l1) (rest l2)))))

(defun sublists (l)
    (if (null l) l (cons l (sublists (rest l)))))

(defun my-reverse (l)
    (my-reverse-h l nil))

(defun my-reverse-h (ol nl)
    (if (null ol) nl (my-reverse-h (rest ol) (cons (first ol) nl))))

(defun every-other (l)
    (cond ((null l) nil)
        (T (cons (first l) (every-other (rest (rest l)))))))

(defun left-half (l)
    (left-half-h l (/ (length l) 2)))

(defun left-half-h (l n)
    (if (<= n 0) nil (cons (first l) (left-half-h (rest l) (1- n)))))

(defun merge-lists (l1 l2)
    (cond ((null l1) l2)
        ((null l2) l1)
        ((< (first l1) (first l2)) (cons (first l1) (merge-lists (rest l1) l2)))
        (T (cons (first l2) (merge-lists l1 (rest l2))))))

(defun fac (n)
    (cond ((= n 0) (break "n = 0"))
        (T (* n (fac (- n 1))))))

(setf family
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

(defun father (name)
    (second (assoc name family)))

(defun mother (name)
    (third (assoc name family)))

(defun parents (name)
    (union (and (father name) (list (father name)))
        (and (mother name) (list (mother name)))))

(defun children (name)
    (and name (mapcar #'first 
        (remove-if-not #'(lambda (x) (member name (rest x))) family))))

(defun siblings (name)
    (remove name (reduce #'union (mapcar #'children (parents name)))))

(defun mapunion (op lst)
    (and lst (reduce #'union (mapcar op lst))))

(defun grandparents (name)
    (mapunion #'parents (parents name)))

(defun cousins (name)
    (mapunion #'children (mapunion #'siblings (parents name))))

(defun descended-from (name1 name2)
    (cond ((null name1) nil)
        ((member name2 (parents name1)) T) 
        (T (or (descended-from (mother name1) name2)
            (descended-from (father name1) name2)))))

(defun ancestors (name)
    (and name (union (parents name) (mapunion #'ancestors (parents name)))))

(defun generation-gap (y o)
    (and o (descended-from y o) 
        (cond ((member o (parents y)) 1 )
            (T (+ 1 (or (generation-gap (mother y) o) 
                        (generation-gap (father y) o)))))))

(defun count-up (n)
    (count-up-tr n nil))

(defun count-up-tr (n r)
    (if (= n 0) r (count-up-tr (1- n) (cons n r))))

(defun fac (n)
    (if (zerop n) 1 (* n (fac (1- n)))))

(defun fac-tr (n) 
    (labels ((fac-tr-h (n r) (if (= n 0) r (fac-tr-h (1- n) (* n r)))))
        (fac-tr-h n 1)))
    

(defun tr-union (l1 l2)
    (tr-union-h l1 l2 l2))

(defun tr-union-h (l1 l2 r)
    (cond ((null l1) r)
        ((not (member (first l1) l2))
            (tr-union-h (rest l1) l2 (cons (first l1) r)))
        (T (tr-union-h (rest l1) l2 r))))

(defun tr-set-difference (s1 s2)
    (cond ((or (null s1) (null s2)) s1)
        (T (tr-set-difference (remove (first s2) s1) (rest s2)))))

(defun tree-find-if (fn tree)
    (cond ((null tree) nil)
        ((atom (first tree))
            (if (funcall fn (first tree)) (first tree) 
                (tree-find-if fn (rest tree))))
        (T (or (tree-find-if fn (first tree)) (tree-find-if fn (rest tree))))))

(defun tr-count-slices (b)
    (labels ((tr-ct-sl-h (br ct) (if (null br) ct (tr-ct-sl-h (rest br) (1+ ct)))))
        (tr-ct-sl-h b 0)))

(defun tr-reverse (l)
    (labels ((tr-help (l r) 
                (if (null l) r (tr-help (rest l) (cons (first l) r)))))
        (tr-help l nil)))

(defun arith-eval (expr)
    (cond ((numberp expr) expr)
        (T (funcall (second expr) (arith-eval (first expr)) (arith-eval (third expr))))))

(defun legalp (expr)
    (cond 
        ((numberp expr) T)
        ((and (member (second expr) '(+ - / *))
            (legalp (first expr))
            (legalp (third expr))) T)))

(defun factors (n)
    (factors-help n 2))

(defun factors-help (n p)
    (cond ((equal n 1) nil)
        ((zerop (rem n p))
            (cons p (factors-help (/ n p) p)))
        (t (factors-help n (+ p 1)))))

(defun pilots ()
    (format t "~&There ara old Pilots~&And there are bold Pilots~&But there are no old bold Pilots"))

(defun draw-line (n)
    (cond ((< 0 n) (format t "*")(draw-line (1- n)))))

(defun draw-box (w h)
    (cond ((< 0 h) (draw-line w) (format t "~&") (draw-box w (1- h)))))

(defun gross-pay ()
    (format t "~&Please enter hourly wage: ")
    (let ((wage-per-h (read)))
        (format t "~&Please enter work-time in hours: ")
        (let ((time (read)))
            (format t "~&Your earnings : ~A $" (* time wage-per-h)))))

(defun cookie-monster ()
    (format t "~&GIVE ME COOKIE~%Cookie? ")
    (let ((in (read)))
        (if (equal in 'cookie) 
            (format t "~&Thank you.... !!")
            (T (format t "~&I dont want no ~A" in)(cookie-monster)))))

(defun space-over (n)
    (cond ((< 0 n) (format t " ")(space-over (1- n)))
        ((< n 0) (format t "~&ERROR"))))

(defun plot-one-point (plotting-string y-val)
    (space-over y-val)(format t "~A~%" plotting-string)))

(defun plot-points (plotting-string points)
    (mapcar #'(lambda (point) (plot-one-point plotting-string point)) points))

(defun generate (l h)
    (labels ((generate-tr (l h res) 
                (if (> l h) res (generate-tr l (1- h) (cons h res)))))
        (generate-tr l h nil)))

(defun make-graph (f h l p-str)
    (plot-points p-str (mapcar f (generate h l))))

(defun square (x) (* x x))

(defun dot-prin1 (l)
    (cond ((null l) (format t "nil"))
        ((atom l) (format t "~S" l))
        (T (format t "(") (dot-prin1 (first l)) (format t " . ") 
                            (dot-prin1 (rest l)) (format t ")"))))

(defun hybrid-prin1 (l)
    (cond ((consp l) (format t "(") (hybrid-prin1-h l))
        (T (hybrid-prin1-h l))))

(defun hybrid-prin1-h (l)
    (cond ((atom l) (format t "~S" l))
        ((null (rest l)) (hybrid-prin1 (first l)) (format t ")"))
        ((consp (rest l)) 
            (hybrid-prin1 (first l)) (format t " ") (hybrid-prin1-h (rest l)))
        (T (hybrid-prin1 (first l)) (format t " . ") (hybrid-prin1-h (rest l)) (format t ")"))))

(setf friends nil)
(setf met-multiple 0)

(defun meet (n)
    (cond ((equal (first friends) n) 
                    (format t "~&We just met")(incf met-multiple))
                ((member n friends) (format t "~&We know each other")(incf met-multiple))
                (T (push n friends) (format t "~&nice to sweet you"))))

(defun chop (l)
    (setf (cdr l) nil))

(defun ntack (l e)
    (if (= (length l) 1) (setf (cdr l) (list e)) (ntack (rest l) e)))

(defun member-it (e l)
    (dolist (n l)
        (when (equal n e) (return T))))

(defun length-it (lst)
    (let ((len 0))
        (dolist (e lst len)
            (incf len))))

(defun nth-it (n lst)
    (let ((ct n))
        (dolist (e lst)
            (when (zerop ct) (return e))
            (decf ct))))

(defun union-it (s1 s2)
    (let ((res s1))
        (dolist (e s2 res)
            (unless (member e s1) (push e res)))))

(defun it-reverse (lst)
    (let ((res nil))
        (dolist (e lst res)
            (push e res))))

(defun check-all-odd (lst)
    (do ((e (pop lst) (pop lst)))
        ((null e) T)
        (when (evenp e) (return nil))))

(defun launch (n)
    (dotimes (i n (format t "Blast off!!"))
        (format t "~S..." (- n i))))

(defun find-largest (lst)
    (do* ((l lst (rest l))
          (e (first l) (first l))
          (largest e))
         ((null l) largest)
         (when (< largest e) (setf largest e))))

(defun power-of-2 (n)
    (do ((res 1 (* res 2))
        (n1 n (1- n1)))
        ((zerop n1) res)))

(defun first-non-integer (lst)
    (dolist (e lst 'none)
        (unless (integerp e) (return e))))

(defun fib (n)
    (if (< n 2) 1
        (do ((fib1 1 fib2)
            (fib2 1 (+ fib1 fib2))
            (i 2 (1+ i)))
            ((= i n) (+ fib1 fib2)))))

(defun complement-base (b)
    (cond ((equal b 'a) 't)
          ((equal b 't) 'a)
          ((equal b 'g) 'c)
          ((equal b 'c) 'g)))

(defun complement-strand (s)
    "push version"
    (do ((strand s (rest strand))
         (c-strand nil (push (complement-base (first strand)) c-strand)))
        ((null strand) (nreverse c-strand))))

(defun complement-strand (s)
    "cons version"
    (do ((strand s (rest strand))
         (c-strand nil (cons (complement-base (first strand)) c-strand)))
        ((null strand) (nreverse c-strand))))

(defun make-double (s)
    (do ((strand s (rest strand))
         (compl-strand (complement-strand s) (rest compl-strand))
         (d-strand nil (cons (list (first strand) (first compl-strand)) d-strand)))
        ((null strand) (nreverse d-strand))))

(defun count-bases (s)
    (do ((ct '((A 0)(T 0)(G 0)(C 0)))
         (strand s (rest strand)))
        ((null strand) ct)
        (when (atom (first strand))
            (incf (second (assoc (first strand) ct))))
        (when (consp (first strand))
            (incf (second (assoc (first (first strand)) ct)))
            (incf (second (assoc (second (first strand)) ct))))))

(defun prefixp (s1 s2)
    (do ((strand1 s1 (rest strand1))
         (strand2 s2 (rest strand2)))
        ((null strand1) t)
        (unless (equal (first strand1) (first strand2)) (return nil))))

(defun appersp (s1 s2)
    (do ((strand2 s2 (rest strand2)))
        ((null strand2) nil)
        (when (prefixp s1 strand2) (return t))))

(defun coverp (s1 s2)
    (do ((strand2 s2 (nthcdr (length s1) strand2)))
        ((null strand2) t)
        (unless (prefixp s1 strand2) (return nil))))

(defun prefix (n s)
    (do ((p-strand nil (cons (first strand) p-strand))
         (strand s (rest strand))
         (i n (1- i)))
        ((zerop i) (nreverse p-strand))))

(defun kernel (strand)
    (do* ((n 1 (1+ n)) 
          (k-strand (prefix n strand) (prefix n strand)))
        ((coverp k-strand strand) k-strand)))

(defun draw-dna (strand)
    (let ((ct (length strand)))
        (dotimes (n ct)
            (format t "-----"))
        (format t "~%")
        (dotimes (n ct)
            (format t "  !  "))
        (format t "~%")
        (dolist (e strand)
            (format t "  ~S  " e))
        (format t "~%")
        (dotimes (n ct)
            (format t "  .  "))
        (format t "~%")
        (dotimes (n ct)
            (format t "  .  "))
        (format t "~%")
        (dolist (e (complement-strand strand))
            (format t "  ~S  " e))
        (format t "~%")
        (dotimes (n ct)
            (format t "  !  "))
        (format t "~%")
        (dotimes (n ct)
            (format t "-----"))))



(defstruct node name question yes-case no-case)

(defun init () 
    (setf *nodelist* nil))

(defun add-node (name question yes-case no-case)
    (push 
        (make-node 
            :name name
            :question question
            :yes-case yes-case
            :no-case no-case) 
        *nodelist*))

(add-node 'start
	  "Does the engine turn over?"
	  'engine-turns-over
	  'engine-wont-turn-over)

(add-node 'engine-turns-over
	  "Will the engine run for any period of time?"
	  'engine-will-run-briefly
	  'engine-wont-run)

(add-node 'engine-wont-run
	  "Is there gas in the tank?"
	  'gas-in-tank
	  "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
	  "Do you hear any sound when you turn the key?"
	  'sound-when-turn-key
	  'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
	  "Is the battery voltage low?"
	  "Replace the battery"
	  'battery-voltage-ok)

(add-node 'battery-voltage-ok
	  "Are the battery cables dirty or loose?"
	  "Clean the cables and tighten the connections."
	  'battery-cables-good)


(defun find-node (name)
    (dolist (e *nodelist*)
        (when (equal (node-name e) name) (return e))))

(defun process-node (name)
    (let ((node (find-node name)))
        (if node 
                (if (y-or-n-p "~&~A~%" (node-question node))
                    (node-yes-case node)
                    (node-no-case node))
            (format t "~&~S not yet defined" name))))

(defun run ()
    (do ((current-node 'start (process-node current-node)))
        ((or (stringp current-node) (null current-node)) 
            (when current-node (format t "~&~A" current-node)))))

(defun subprop (sym val prop)
    (setf (get sym prop) (delete val (Get sym prop))))

(defun my-get (sym prop &optional not-found)
    (do* ((plist (symbol-plist sym) (rest (rest plist)))
          (pnam (first plist) (first plist)))
          ((null plist) not-found)
        (when (eq pnam prop) (return (second plist)))))


(defmacro simple-rotatef (a b)
    `(let ((t1 (setf ,a ,b))
          (t2 (setf ,b ,a)))))

(defmacro set-mutual (a b)
    `(progn (setf ,a ',b)(setf ,b ',a)))

(defmacro variable-chain (&rest vars)
    `(progn ,@(do ((seq vars (rest seq))
                   (res nil (push `(setf ,(first seq) ',(second seq)) res)))
                   ((null (second seq)) res))))
