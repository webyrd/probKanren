(run 10 (q) (fresh (uniform 0 1) (x) (flip x #t)))

(run 10 (q) (fresh (uniform 0 1) (x) (flip x #t) (== q x)))

(run 10 (uniform 0 1) (x) (flip x #t))

(run-with-distribution10 (uniform 0 1) (x) (flip x #t))

(mh-query
  (define x (uniform 0 1))
  (define q (flip x))
  q
  (eq x #t))
          
(mh-query
  (define x (uniform 0 1))
  (define q (flip x))
  x
  (eq q #t))

(run-mh (uniform 0 1) (x)
  (fresh (q)
    (flip x q)
    (== q #t)))

(run-mh (uniform 0 1) (x)
  (fresh (flip x) (q)
    (== q #t)))

(run-mh (x)
  (fresh (q)
    (flip x q)
    (== q #t))
  (uniform 0 1 x))

(run-mh (q)
  (uniform 0 1 q)
  (normal 0 1 q)
  (== q -1))
