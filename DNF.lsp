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

(defun _mapDeMorgan (A)
  (cond
    ((eq A '+) '*)
    ((eq A '*) '+)
    (t nil)))

; if invoked on list from ! (list) changes it to correct deMorganed list
; ! (! a) -> (_deMorganNeg '(! a)) -> a
; ! (! (a + b)) -> (_deMorganNeg '(! (a + b))) -> (a + b)
(defun _deMorganNeg (L)
  (head2 L))

; if invoked on list that has binary operator changes it to correct deMorganed list
; ! (a + b) -> ((! a) * (! b))
(defun _deMorganBin (L)
  (conser (conser '! (head L)) (_mapDeMorgan (head2 L)) (conser '! (head3 L))))

(defun _deMorgan (L)
  (cond
    ((IsNeg (head L)) (_deMorganNeg L))
    ((or (IsCon (head2 L)) (IsDis (head2 L))) (_deMorganBin L))
    (t (format t "~%STRANGE THINGS: ~A~%" L))))

; implies de Morgans rules on list
(defun DropMorgans (L)
  (defun re (X) (DropMorgans X))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((IsNeg (head L))
     (cond
       ((atom (head2 L)) L)
       (t (re (_deMorgan (head2 L))))))
    (t (mapcar (lambda (A) (re A)) L))))
