;


(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(load "demorgan.lsp")
(load "dnf.lsp")
(load "blake.lsp")

(defun main (L)
  (_drops (Transform (Modify (Formula L)))))
; (TEST_UTILS)
; (TEST_FORMULA)
; (TEST_DEMORGAN)
; (TEST_FORMULA)
; (TEST_DNF)
(TEST_BLAKE)
; (print (main '(a > (1 + (c * (! d)) + 0 * f))))
(print (main '(! a + b * 1 * 0 + c > b)))

