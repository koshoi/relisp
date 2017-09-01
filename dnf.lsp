;
(load "utils.lsp")
(load "formula.lsp")

;e_c is a structure that looks like this
;(e_c (set)), where set is a set of atoms and (! atoms) that are in elemnatry conjunction

(defun IsELCon (L)
  (equal (head L) 'e_c))

(defun IsSimple (A)
  (cond
    ((null A) t)
    ((atom A) t)
    ((and (IsNeg (head A)) (atom (head2 A))) t)
    (t nil)))

(defun _makeElCon (L)
  (cond
    ((IsELCon L) L)
    ((atom L) (conser 'e_c (list L)))
    ((IsNeg (head L)) (conser 'e_c (list L)))
    ((IsCon (head2 L)) (_mergeElCons (_makeElCon (head L)) (_makeElCon (head3 L))))))

(defun _mergeElCons (A B)
  (conser 'e_c (merge_sets (head2 A) (head2 B))))
