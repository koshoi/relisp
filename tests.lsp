;


(defun IS (A B N)
  (cond
    ((equal A B) t)
    (t (format t "~%FAILED ~A~%<<<GOT      ~A~%>>>EXPECTED ~A~%" N A B))))

(defun IS_NOT (A B N)
  (cond
    ((not (equal A B)) t)
    (t (format t "~%FAILED ~A~%<<<GOT      ~A~%>>>EXPECTED ~A~%"  N))))

(defun TEST_UTILS ()
  (load "utils.lsp")
  (print "UTILS TESTING")
  (IS '(a b) '(a b) "test of test1")
  (IS '(a (a b)) '(a (a b)) "test of test3")
  (IS '(a (a b) (a) b) '(a (a b) (a) b) "test of test4")
  (IS_NOT '(a b) '(a c) "test of test5")
  (IS_NOT '(a (a b) (a) b) '(a (a b) () b) "test of test6")

  (IS (head 'a) 'a "head test1")
  (IS (head '(a)) 'a "head test2")
  (IS (head '()) nil "head test3")
  (IS (head '((a b) b)) '(a b) "head test4")

  (IS (tail 'a) nil "tail test1")
  (IS (tail '(a b)) '(b) "tail test2")
  (IS (tail '(a (a b))) '((a b)) "tail test3")
  (IS (tail '(a)) nil "tail test4")

  (IS (len '()) 0 "len test1")
  (IS (len '(a)) 1 "len test2")
  (IS (len '(a b)) 2 "len test3")
  (IS (len '(a () (b a b))) 3 "len test4")

  (IS (take 2 '()) nil "take test1")
  (IS (take 7 '(a b c d e f g)) 'g "take test2")
  (IS (take 0 '(a b c)) nil "take test3")
  (IS (take 1 '(() a)) '() "take test4")
  (IS (take -1 '(a b)) nil "take test5")
  (IS (take 2 '((a b) (b c))) '(b c) "take test6")

  (IS (drop 0 '(a b)) '(a b) "drop test1")
  (IS (drop 1 '(a b c)) '(b c) "drop test2")
  (IS (drop 2 '((a b) (a ()) (a b) b)) '((a b) b) "drop test3")

  (IS (mylist 'a) '(a) "mylist test1")
  (IS (mylist '()) nil "mylist test2")
  (IS (mylist '(a)) '(a) "mylist test3")
  (IS (mylist '(a b)) '(a b) "mylist test4")
  (print "DONE TESTING"))

(defun TEST_FORMULA ()
  (load "formula.lsp")
  (print "TESTING FORMULA")
  (IS (IsNeg ()) nil "IsNeg test1")
  (IS (IsNeg '1) nil "IsNeg test2")
  (IS (IsNeg '!) t "IsNeg test3")

  (IS (DropNegatives '(a)) '(a) "DropNeg test1")
  (IS (DropNegatives '(a b)) '(a b) "DropNeg test2")
  (IS (DropNegatives '()) '() "DropNeg test3")
  (IS (DropNegatives '(! ! a)) '(a) "DropNeg test4")
  (IS (DropNegatives '(! a)) '(! a) "DropNeg test5")
  (IS (DropNegatives '(! ! ! a)) '(! a) "DropNeg test6")
  (IS (DropNegatives '(! ! ! a ! b ! ! c)) '(! a ! b c) "DropNeg test7")
  (IS (DropNegatives '(! ! (! a) ! b ! ! c)) '((! a) ! b c) "DropNeg test8")
  (IS (DropNegatives '(! ! (! a) ((! b)) ! ! c)) '((! a) ((! b)) c) "DropNeg test9")
  (IS (DropNegatives '(! ! ! ! ! ! ! !)) '() "DropNeg test10")
  (IS (DropNegatives '(((! ! a)) b c ! ! a)) '(((a)) b c a) "DropNeg test11")

  (IS (TakeNegatives '()) '() "TakeNegatives test1")
  (IS (TakeNegatives '(a)) '(a) "TakeNegatives test2")
  (IS (TakeNegatives '((a))) '((a)) "TakeNegatives test3")
  (IS (TakeNegatives '((! a))) '(((! a))) "TakeNegatives test4")
  (IS (TakeNegatives '(a b)) '(a b) "TakeNegatives test5")
  (IS (TakeNegatives '(! a b)) '((! a) b) "TakeNegatives test6")
  (IS (TakeNegatives '((((! a b))))) '(((((! a) b)))) "TakeNegatives test7")
  (IS (TakeNegatives '((((! a b)) c) ! d)) '(((((! a) b)) c) (! d)) "TakeNegatives test8")
  (IS (TakeNegatives '((((! a b)) c f ! g) ! d)) '(((((! a) b)) c f (! g)) (! d)) "TakeNegatives test9")

  (IS (FormLevel '() '*) nil "FormLevel test1")
  (IS (FormLevel '(a) '*) '(a) "FormLevel test2")
  (IS (FormLevel '(a * b) '*) '((a * b)) "FormLevel test3")
  (IS (FormLevel '(a + b * c) '*) '(a + (b * c)) "FormLevel test4")
  (IS (FormLevel '(a * b * c) '*) '(((a * b) * c)) "FormLevel test5")
  (IS (FormLevel '(1 + a * b * c) '*) '(1 + ((a * b) * c)) "FormLevel test6")
  (IS (FormLevel '(1 + a * (b + 0) * c) '*) '(1 + ((a * (b + 0)) * c)) "FormLevel test7")
  (IS (FormLevel '(1 + (((! a))) * (b + 0) * c) '*) '(1 + (((((! a))) * (b + 0)) * c)) "FormLevel test8")
  (print "DONE TESTING"))

(defun TEST_DNF ()
  (load "DNF.lsp")
  (print "START DNF TESTING")

  (IS (DropImplic '(a > b)) '((! a) + b) "DropImplic test1")
  (IS (DropImplic '(a > b * c)) '((! a) + (b * c)) "DropImplic test2")
  (IS (DropImplic '(a > (b > c))) '((! a) + ((! b) + c)) "DropImplic test3")
  (IS (DropImplic '((((a > b))))) '((! a) + b) "DropImplic test4")

  (print "DONE TESTING"))
