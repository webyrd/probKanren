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
     (uniform 0 1 #(x) . #<procedure [char 5096 of mk.scm]>))))

(test "run-mh-3"
  (run-mh (x)
    (fresh (b)
      (flip x b)
      (== b #t))
    (uniform 0 1 x))
  '((_.0
     (uniform 0 1 #(x) . #<procedure [char 5096 of mk.scm]>)
     (flip #(x) #(b) . #<procedure [char 4808 of mk.scm]>))))
