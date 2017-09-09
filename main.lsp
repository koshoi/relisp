;

(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(load "demorgan.lsp")
(load "dnf.lsp")
(load "blake.lsp")

(defun main (L)
  (Blake (print (Transform (Modify (Formula L))))))
; (TEST_UTILS)
; (TEST_FORMULA)
; (TEST_DEMORGAN)
; (TEST_FORMULA)
; (TEST_DNF)
(TEST_BLAKE)
(print (main '(a * ! a > b * ! b)))

