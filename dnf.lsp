;
(load "utils.lsp")
(load "formula.lsp")

;elementary_conj is a structure that looks like this
;(elementary_conj (set)), where set is a set of atoms and (! atoms) that are in elemnatry conjunction

(defun IsELCon (L)
  (equal (head L) 'elementary_conj))

(defun DropDepth (L)
  (defun re (X) (DropDepth X))
  (cond
    ((null L) nil)
    ((atom L) L)
    (t nil)))

(defun _makeElCon (L)
  (cond
    ((IsELCon L) L)
    ((atom L) (conser 'elementary_conj (list L)))
    ((IsNeg (head L)) (conser 'elementary_conj (list L)))
    ((IsCon (head2 L)) (_mergeElCons (_makeElCon (head L)) (_makeElCon (head3 L))))))

(defun _mergeElCons (A B)
  (conser 'elementary_conj (merge_sets (head2 A) (head2 B))))
