;

(load "utils.lsp")
(load "formula.lsp")

(defun _drops (L)
  (_dropAntipodes (_dropWithZeros (_dropOnes (_negConstants L)))))

; (a b) (c d 0) -> (a b)
(defun _dropWithZeros (L)
  (defun is_ok (X) (cond ((is_in '0 X) nil) (t (list X))))
  (mapcan (lambda (Y) (is_ok Y)) L))

; (a b 1) -> (a b)
(defun _dropOnes (L)
  (defun localdrop (X) (mapcan (lambda (Y) (cond ((eq Y '1) nil) (t (list Y)))) X))
  (mapcar (lambda (Z) (cond ((eq (len Z) 1) Z) (t (localdrop Z)))) L))

; (a b) (c d (! c)) -> (a b)
(defun _dropAntipodes (L)
  (defun antipode (X) (cond ((IsNeg (head X)) (head2 X)) (t (conser '! x))))
  (defun goodlist (X)
    (cond
      ((null X) t)
      (t (and (not (is_in (antipode (head X)) (tail X))) (goodlist (tail X))))))
  (mapcan (lambda (Y) (cond ((goodlist Y) (list Y)) (t nil))) L))

; (a (! 0)) -> (a 1)
(defun _negConstants (L)
  (defun swap (X) (cond ((and (IsNeg (head X)) (is_const (head2 X))) (un (head2 X))) (t X))) 
  (defun un (X) (cond ((eq X 1) 0) (t 1)))
  (defun is_const (X) (or (eq X 1) (eq X 0)))
  (mapcar (lambda (Y) (mapcar (lambda (Z) (swap Z)) Y)) L))

; (a b) (c d) -> nil
; (a b) (a c) -> (a)
; (a b c) (a c d) -> (a b)
(defun _intersections (A B)
  (cond
    ((null A) nil)
    ((null B) nil)
    ((is_in (head A) B) (append (list (head A)) (_intersections (tail A) B)))
    (t (_intersections (tail A) B))))

; (a b) (c d) -> ()
; (a b) (a d) -> (a b) (a d)
; (a b) ((! a) d) -> (a b) ((! a) d) (b d)
; (a b c) ((! a) d) -> (a b c) ((! a) d) (b c d)
; (a b c) ((! a) (! b) d) -> (a b c) ((! a) (! b) d) (b (! b) c d) (a c (! a) d)
(defun _intersect (A B)
  (defun re (X Y) (_intersect X Y))
  (defun _d (X Y) (dropFromSet X Y))
  (defun antipode (X) (cond ((IsNeg (head X)) (head2 X)) (t (conser '! x))))
  (defun _neglist (L) (mapcar (lambda (X) (antipode X)) L))
  (defun f (L) (cond ((null L) nil) (t (list L))))
  (append
    (conser A B)
    (mapcan (lambda (X) (f (merge_sets (_d X A) (_d (antipode X) B)))) (_intersections A (_neglist B)))))

(defun _intersectAll (L)
  (defun recall (X) (_intersectAll X))
  (cond
    ((null L) nil)
    ((eq (len L) 1) nil)
    (t (append (mapcan (lambda (X) (_intersect (head L) X)) (tail L)) (recall (tail L))))))

; returns bool whether A can absorb B
; (a) (a b) -> t
; (a) (c b) -> nil
(defun _canAbsorb (A B)
  (reduce (lambda (X Y) (and X Y)) (mapcar (lambda (Z) (is_in Z B)) A)))

; absorbs everything absorbable with first element of the list
(defun _makeAbsorbs (L)
  (defun re (X) (_makeAbsorbs X))
  (defun absorb (A B) (cond ((_canAbsorb A B) nil) (t (list B))))
  (defun f (X) (append (list (head X)) (mapcan (lambda (Y) (absorb (head X) Y)) (tail X))))
  (cond
    ((null L) nil)
    (t (append (list (head L)) (re (tail (f L)))))))

(defun _blake (L)
  (reverse (_makeAbsorbs (reverse (_makeAbsorbs (_intersectAll L))))))

(defun Blake (L)
  (_blake (_drops L)))

