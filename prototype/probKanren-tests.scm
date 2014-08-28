(load "mkdefs.scm")
(load "test-check.scm")

(test "run-mh-1"
  (run-mh (x)
    (== x 5))
  '(5))

(test "run-mh-2"
  (run-mh (x)
    (uniform 0 1 x))
  '((_.0
     (query-variable #(x))
     (subst ())
     (uniform 0 1 #(x) . #<procedure [char 5154 of mk.scm]>))))

(test "run-mh-3"
  (run-mh (x)
    (fresh (b)
      (flip x b)
      (== b #t))
    (uniform 0 1 x))
  '((_.0
     (query-variable #(x))
     (subst ((#(b) . #t)))
     (uniform 0 1 #(x) . #<procedure [char 5154 of mk.scm]>)
     (flip #(x) #t . #<procedure [char 4866 of mk.scm]>))))
