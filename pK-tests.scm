(load "mk.scm")
(load "delayed-goal-defs.scm")
(load "test-check.scm")

(test-random "uniform-1"
  (run* (q) (uniform 0.0 1.0 q))
  '(0.4530346801796503))

(test-random "uniform-2"
  (run* (q) (fresh (x y) (uniform x y q) (== 0.0 x) (== 1.0 y)))
  '(0.4530346801796503))

(test-random "uniform-3"
  (run* (x y z) (uniform x y z) (== 0.0 x) (== 1.0 y))
  '((0.0 1.0 0.4530346801796503)))

(test-random "uniform-4"
  (run* (x y z) (== 0.0 x) (== 1.0 y) (uniform x y z))
  '((0.0 1.0 0.4530346801796503)))


(test-random "flip-1"
  (run* (q) (flip 0.5 q))
  '(#t))

(test-random "flip-2"
  (run* (q) (flip 0.1 q))
  '(#f))

(test-random "flip-3"
  (run* (q) (fresh (x) (== x 0.1) (flip x q)))
  '(#f))

(test-random "flip-4"
  (run* (q) (fresh (x) (flip x q) (== x 0.1)))
  '(#f))

(test-random "flip-5"
  (run* (b x)
    (flip 0.5 b)
    (conde
      [(== #t b) (== 'w00t x)]
      [(== #f b) (== 'barf x)]))
  '((#t w00t)))


(test-random "uniform-flip-1a"
  (run* (x q)
    (uniform 0.0 1.0 x)
    (flip x q))
  '((0.4530346801796503 #t)))

(test-random "uniform-flip-1b"
  (run* (x q)
    (flip x q)
    (uniform 0.0 1.0 x))
  '((0.4530346801796503 #t)))

(test-random "uniform-flip-2"
  (run* (x)
    (uniform 0.0 1.0 x)
    (flip x #t))
  '(0.4530346801796503))

(test-random "normal-1"
  (run* (x)
    (normal 0.0 1.0 x))
  '(-1.1740941342295155))

(test "normal-density-1"
  (run* (q)
    (normal-density 0.0 0.0 1.0 q))
  '(-0.9189385332046727))

(test "normal-density-2"
  (run* (q)
    (normal-density 3.0 2.0 5.0 q))
  '(-2.548376445638773))
