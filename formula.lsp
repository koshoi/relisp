;

(load "utils.lsp")

(defun IsNeg (A)
  (eq A '!))

(defun IsCon (A)
  (eq A '*))

(defun IsDis (A)
  (eq A '+))

(defun IsImp (A)
  (eq A '>))

; ! ! a -> a
(defun DropNegatives (L)
  (defun re (X) (DropNegatives X))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((and (IsNeg (head L)) (IsNeg (head2 L))) (re (tail2 L)))
    (t (append (list (re (head L))) (re (tail L))))))

; ! a b -> (! a) b
(defun TakeNegatives (L)
  (defun re (X) (TakeNegatives X))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((IsNeg (head L)) (append (lister '! (re (head2 L))) (re (tail2 L))))
    (t (append (list (re (head L))) (re (tail L))))))

; a + b Op c -> a + (formed b Op c)
(defun _formLevel (L Op)
  (defun re (X) (_formLevel X Op))
  (defun is_op (X) (eq (head2 X) Op))
  (defun level (X) (re (lister 'formed (same (head X)) (head2 X) (same (head3 X)))))
  (defun formed (X) (eq (head X) 'formed))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((formed L) L)
    ((is_op L) (re (append (level L) (re (tail3 L)))))
    (t (append (list (re (head L))) (re (tail L))))))

; fomed a Op b -> a Op b
(defun _dropFormed (L)
  (defun re (X) (_dropFormed X))
  (defun formed (X) (eq (head X) 'formed))
  (cond
    ((null L) nil)
    ((atom L) L)
    ((formed L) (_dropFormed (tail L)))
    (t (mapcar (lambda (A) (re A)) L))))

(defun FormLevel (L Op)
  (_dropFormed (_formLevel L Op)))

(defun _formula (L Ops)
  (defun re (X) (_formula X (tail Ops)))
  (cond
    ((null Ops) L)
    (t (re (FormLevel L (head Ops))))))

; forms formula with following priorities * -> + -> >
(defun Formula (L)
  (_formula (TakeNegatives (DropNegatives L)) '(* + >)))
