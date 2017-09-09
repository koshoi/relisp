;

(defun head (L)
  (cond
    ((null L) nil)
    ((atom L) L)
    (t (car L))))

(defun head2 (L)
  (take 2 L))

(defun head3 (L)
  (take 3 L))

(defun take (N L)
  (cond
    ((<= N 0) nil)
    ((eq N 1) (head L))
    (t (take (- N 1) (tail L)))))

(defun drop (N L)
  (cond
    ((<= N 0) L)
    ((eq N 1) (tail L))
    (t (drop (- N 1) (tail L)))))

(defun tail (L)
  (cond
    ((null L) nil)
    ((atom L) nil)
    (t (cdr L))))

(defun tail2 (L)
  (drop 2 L))

(defun tail3 (L)
  (drop 3 L))

(defun len (L)
  (cond
    ((null L) 0)
    (t (+ 1 (len (tail L))))))

(defun mylist (L)
  (cond
    ((null L) nil)
    ((atom L) (list L))
    (t L)))

(defun lister (&rest L)
  (list L))

(defun conser (&rest L)
  L)

(defun same (L)
  L)

(defun merge_sets (A B)
  (defun re (X Y) (merge_sets X Y))
  (cond
    ((null B) A)
    ((is_in (head B) A) (re A (tail B)))
    (t (re (append A (list (head B))) (tail B)))))

(defun is_in (A L)
  (cond
    ((null L) nil)
    ((equal (head L) A) t)
    (t (is_in A (tail L)))))

(defun dropFromSet (A L)
  (mapcan (lambda (X) (cond ((equal A X) nil) (t (list X)))) L))
