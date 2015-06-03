(load "mk.scm")
(load "helpers.scm")
(load "test-check.scm")

(test-random "repeato-1"
  (run-mh 10 (q) (repeato 3 (lambda (x) (flip 0.25 x)) q))
  '((#f #f #f)
    (#f #f #f)
    (#f #f #f)
    (#f #f #f)
    (#f #f #f)
    (#f #f #f)
    (#f #f #f)
    (#f #f #f)
    (#t #f #f)
    (#f #f #f)))

(test "seq-goalo-1"
  (run-mh 4 (q) (fresh (x y z) (seq-goalo (lambda (x) (== x #t)) (list x y z)) (== (list x y z) q)))
  '((#t #t #t) (#t #t #t) (#t #t #t) (#t #t #t)))

(test "map-goalo-1"
  (run-mh 3 (q)
    (map-goalo (lambda (x out)
                 (== x out))
               (list 1 2 3)
               q))
  '((1 2 3) (1 2 3) (1 2 3)))

(define >o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (> x y) z)))))

(test "zipwitho-1"
  (run-mh 4 (out) (zipwitho >o '(1 2 3) '(4 1 6) out))
  '((#f #t #f) (#f #t #f) (#f #t #f) (#f #t #f)))
