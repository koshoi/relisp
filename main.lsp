;


(load "utils.lsp")
(load "tests.lsp")
(load "formula.lsp")
(TEST_UTILS)
(TEST_FORMULA)
(print (DropNegatives '(a ! ! ! ! b ! ! C)))
