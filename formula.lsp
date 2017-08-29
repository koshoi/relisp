;

(load "utils.lsp")

(defun IsNeg (A)
  (eq A '!))

(defun DropNegatives (L)
  (defun re (X) (DropNegatives X))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((and (IsNeg (head L)) (IsNeg (head2 L))) (re (tail2 L)))
    (t (append (list (re (head L))) (re (tail L))))))

(defun TakeNegatives (L)
  (defun re (X) (TakeNegatives X))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((IsNeg (head L)) (append (lister '! (re (head2 L))) (re (tail2 L))))
    (t (append (list (re (head L))) (re (tail L))))))
