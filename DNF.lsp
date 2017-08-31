;

(load "formula.lsp")
(load "utils.lsp")

(defun DropImplic (L)
  (defun re (X) (DropImplic X))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((eq (len L) 1) (same (re (head (same L)))))
    ((IsNeg (head L)) (conser '! (re (head2 L))))
    ((IsImp (head2 L)) (conser (conser '! (re (head L))) '+ (re (tail2 L))))
    (t (conser (re (head L)) (head2 L) (re (head3 L))))))
