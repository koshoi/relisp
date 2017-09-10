;

(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(load "demorgan.lsp")
(load "dnf.lsp")
(load "blake.lsp")
(load "formatter.lsp")

(defun main (L)
  (FinalFormat (Blake (Transform (Modify (Formula L))))))

; (TEST_UTILS)
; (TEST_FORMULA)
; (TEST_DEMORGAN)
; (TEST_FORMULA)
; (TEST_DNF)
(TEST_BLAKE)
; (TEST_FORMAT)
(print (main '(a * ! a > b * ! b + c + d * e + e * b + 0)))

