;
(load "utils.lsp")
(load "formula.lsp")

;e_c is a structure that looks like this
;(e_c (set)), where set is a set of atoms and (! atoms) that are in elemnatry conjunction

(defun IsElCon (L)
  (equal (head L) 'e_c))

(defun IsSimple (A)
  (cond
    ((null A) t)
    ((atom A) t)
    ((and (IsNeg (head A)) (atom (head2 A))) t)
    (t nil)))

(defun IsSimpleList (L)
  (reduce (lambda (A B) (and A B)) (mapcar (lambda (X) (IsSimple X)) L)))

(defun _makeElCon (L)
  (cond
    ((IsElCon L) L)
    ((atom L) (conser 'e_c L))
    ((IsNeg (head L)) (conser 'e_c L))
    ((IsCon (head2 L)) (_mergeElCons (_makeElCon (head L)) (_makeElCon (head3 L))))))

(defun _mergeElCons (A B)
  (append (list 'e_c) (merge_sets (tail A) (tail B))))

; Transforms every element to e_c structure
; a -> (e_c (a))
; (! a) -> (e_c ((! a)))
; + -> +
; * -> *
(defun _startElCons (L)
  (defun re (X)
    (cond
      ((or (IsDis X) (IsCon X)) X)
      ((IsSimple X) (_makeElCon X))
      (t (conser (_startElCons (head X)) (head2 X) (_startElCons (head3 X))))))
  (cond
    ((IsSimple L) (_makeElCon L))
    (t (same (mapcar (lambda (Y) (re Y)) (mylist L))))))

(defun _mergeSetsElCons (A B)
  (defun pretty (X) (mapcan (lambda (Y) Y) X))
  (pretty (mapcar (lambda (X) (mapcar (lambda (Y) (_mergeElCons X Y)) B)) A)))


; makes set of elementary conjunctions of formula
(defun CollectElCons (L)
  (defun recall (X) (CollectElCons X))
  (cond
    ((IsElCon L) (conser L))
    ((IsDis (head2 L)) (append (recall (head L)) (recall (head3 L))))
    ((IsCon (head2 L)) (_mergeSetsElCons (recall (head L)) (recall (head3 L))))
    (t L)))

; takes formula with ! + * only and transforms it to sets that implicate elementary conjunctions
(defun Transform (L)
  (mapcar (lambda (X) (tail X)) (CollectElCons (_startElCons L))))

