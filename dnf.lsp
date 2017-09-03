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

(defun IsSimpleList (L)
  (reduce (lambda (A B) (and A B)) (mapcar (lambda (X) (IsSimple X)) L)))

(defun _makeElCon (L)
  (cond
    ((IsELCon L) L)
    ((atom L) (conser 'e_c (list L)))
    ((IsNeg (head L)) (conser 'e_c (list L)))
    ((IsCon (head2 L)) (_mergeElCons (_makeElCon (head L)) (_makeElCon (head3 L))))))

(defun _mergeElCons (A B)
  (conser 'e_c (merge_sets (head2 A) (head2 B))))

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
; (((E_C (A B)) (E_C (A C))) ((E_C ((! A) B)) (E_C ((! A) C))))
; (((E_C (A B)) (E_C (A C))) ((E_C ((! A) B)) (E_C ((! A) C))))


(defun CollectElCons (L)
  (defun re (X) (CollectElCons X))
  (cond
    ((IsElCon L) (conser L))
    ((IsDis (head2 L)) (conser (re (head L)) (re (head3 L))))
    ((IsCon (head2 L)) (_mergeSetsElCons (re (head L)) (re (head3 L))))
    (t (format t "STRANGE THING ~A~%" L))))
