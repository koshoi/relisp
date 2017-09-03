;


(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(load "demorgan.lsp")
(load "dnf.lsp")

(defun main (L)
  (Transform (Modify (Formula L))))
; (TEST_UTILS)
; (TEST_FORMULA)
; (TEST_DEMORGAN)
; (TEST_FORMULA)
(TEST_DNF)
(print (main '(a > (1 + (c * (! d)) + 0 * f))))

