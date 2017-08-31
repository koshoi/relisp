;


(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(load "DNF.lsp")
(TEST_UTILS)
(TEST_FORMULA)
(TEST_DNF)
(print (DropImplic (print (Formula '(a + b * c > d > 1)))))

