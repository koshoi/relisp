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
  (dro 3 L))

(defun len (L)
  (cond
    ((null L) 0)
    (t (+ 1 (len (tail L))))))
