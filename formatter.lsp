;

; (a b) * -> (a * b)
; (a b c) * -> (a * b * c)
; (a b c) + -> (a + b + c)
(defun _addOpers (L OP)
  (reverse (drop 1 (reverse (mapcan (lambda (X) (conser X OP)) L)))))

; nil -> (0)
(defun _isFinalFalse (L)
  (cond
    ((null L) '((0)))
    (t L)))

; ((a b) (c d)) -> ((a b) (c d))
; ((a b) (c d) (1)) -> (1)
(defun _isFinalTrue (L)
  (cond
    ((is_in '(1) L) '((1)))
    (t L)))

(defun FinalFormat (L)
  (_addOpers (mapcar (lambda (X) (_addOpers X '*)) (_isFinalFalse (_isFinalTrue L))) '+))

