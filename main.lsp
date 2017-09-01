;


(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(load "demorgan.lsp")
; (TEST_UTILS)
; (TEST_FORMULA)
(TEST_DEMORGAN)
; (print (DropMorgans (print (DropImplic (Formula '(a + b * c > d > 1))))))
; (print (DropMorgans (DropImplic (Formula '((! a) > b > c)))))
; (print (DropMorgans (DropImplic (Formula '((((! a))) > b > c)))))

