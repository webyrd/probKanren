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

