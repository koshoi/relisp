;


(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(TEST_UTILS)
(TEST_FORMULA)
(print (Formula '(a + (b + c * (d + e + 1) * g) + 0 + 1 * 0)))
