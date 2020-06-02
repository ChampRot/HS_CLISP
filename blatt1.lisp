;; Aufgabe 1 a)

(defun my-reverse (lst)
  (when lst (append (my-reverse (rest lst)) (list (first lst)))))

;; Aufgabe 1 b)

(defun my-reverseR (lst)
  (cond ((= (length lst) 1) lst)
        ((consp (first lst))
            (append (my-reverse (rest lst))
                    (list (my-reverse (first lst))))
        ((atom (first lst))
            (append (my-reverse (rest lst))
                    (list (first lst)))))))


;; Aufgabe 2 a)

;; empty-tree -> nil
;; node -> (value left-node right-node) 
 ;;   oder (left-node value right-node)

;; Aufgabe 3

(defun insert (tree val)
  (cond ((null tree) (list val nil nil))
        ((= val (first tree)) tree)
        ((< val (first tree)) 
            (list (first tree) 
                  (insert (second tree) val)
                  (third tree)))
        (t (list (first tree)
                 (second tree)
                 (insert (third tree) val)))))

(defun contains (tree val)
  (cond ((null tree) nil)
        ((= val (first tree)) (first tree))
        ((< val (first tree)) (contains (second tree) val))
        ((> val (first tree)) (contains (third tree) val))))

(defun height (tree)
  (cond ((null tree) 0)
        (t (let ((height-left (height (second tree)))
                 (height-right (height (third tree))))
              (1+ (max height-left height-right))))))

(defun size (tree)
  (cond ((null tree) 0)
        (t (+ 1 (size (second tree)) (size (third tree))))))

(defun getmax (tree)
  (and tree 
      (or (getmax (third tree))
          (first tree))))

(defun getmin (tree)
  (and tree
       (or (getmin (second tree))
           (first tree))))

(defun isempty (tree)
  tree)

(defun remove-t (tree val)
  (cond ((null tree) nil)
        ((= val (first tree))
            (cond ((third tree)
                    `(,(getmin (third tree))
                      ,(second tree)
                      ,(remove-t (third tree)
                                 (getmin (third tree)))))
                  ((second tree)
                    (second tree))
                  (t nil)))
        ((< val (first tree))
          `(,(first tree)
            ,(remove-t (second tree) val)
            ,(third tree)))
        ((> val (first tree))
          `(,(first tree)
            ,(second tree)
            ,(remove-t (third tree) val)))))

(defun add-all (tree other-tree)
  (cond ((null tree) other-tree)
        ((null other-tree) tree)
        (t (add-all 
              (add-all (insert tree (first other-tree))
                       (second other-tree))
              (third other-tree)))))

(defun print-inorder (tree)
  (when tree
    (print-inorder (second tree))
    (princ (first tree))
    (princ " ")
    (print-inorder (third tree))))

(defun print-preorder (tree)
  (when tree
    (princ (first tree))
    (princ " ")
    (print-preorder (second tree))
    (print-preorder (third tree))))

(defun print-postorder (tree)
  (when tree
    (print-postorder (second tree))
    (print-postorder (third tree))
    (princ (first tree))
    (princ " ")))

(defun print-levelorder (tree)
  (labels ((collect-values (node-lst values)
              (if node-lst
                (collect-values (append (rest node-lst) 
                                        (and (second (first node-lst))
                                             (list (second (first node-lst))))
                                        (and (third (first node-lst))
                                             (list (third (first node-lst)))))
                                (append values (list (first (first node-lst)))))
                values)))
    (dolist (val (collect-values (list tree) nil))
            (princ val)
            (princ " "))))

(defparameter test '(1 (2 (4 NIL NIL) (5 NIL NIL)) (3 NIL NIL)))