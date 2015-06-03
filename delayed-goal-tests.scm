(load "mk.scm")
(load "delayed-goal-defs.scm")
(load "test-check.scm")



(test "dg-1"
  (run* (q) (fresh (x) (pluso x 4 q) (== x 3)))
  '(7))

(test "dg-2"
  (run* (q) (fresh (x) (== x 3) (pluso x 4 q)))
  '(7))

(test "dg-3"
  (run* (q) (fresh (x) (pluso x 4 q) (== x 3)))
  '(7))

(test "dg-4"
  (run* (q) (fresh (x) (== x 3) (fresh (y) (pluso x y q) (== y 4))))
  '(7))

(test "dg-5"
  (run* (q) (fresh (x) (fresh (y) (pluso x y q) (== y 4) (== x 3))))
  '(7))


(test "delayed-*o-1"
  (run* (x)
    (*o 3 4 x))
  '(12))

(test "delayed-*o-2"
  (run* (q)
    (fresh (x y)
      (== x 4)
      (*o 3 x y)
      (== `(,x ,y) q)))
  '((4 12)))

(test "delayed-*o-3"
  (run* (q)
    (fresh (x y)
      (*o 3 x y)
      (== x 4)
      (== `(,x ,y) q)))
  '((4 12)))

(test "delayed-*o-4"
  (run* (q)
    (fresh (x y)
      (*o 3 x y)
      (== x 4)
      (== `(,x ,y) q)))
  '((4 12)))

(test "delayed-*o-5"
  (run* (q)
    (fresh (x y)
      (*o 3 x y)
      (== x 4)
      (== `(,x ,y) q)))
  '((4 12)))

(test "delayed-*o-6"
  (run* (q)
    (fresh (x y)
      (*o 3 x y)
      (== x 4)
      (== `(,x ,y) q)))
  '((4 12)))

(test "delayed-*o-7"
  (run* (q)
    (fresh (w x y z)
      (*o x y z)
      (pluso 3 w x)
      (== w 4)
      (== `(,w ,x ,y ,z) q)
      (== y 5)))
  '((4 7 5 35))) 

(test "delayed-*o-8"
  (run* (q)
    (fresh (w x y z)
      (pluso 3 w x)
      (== w 4)
      (== `(,w ,x ,y ,z) q)
      (*o x y z)
      (== y 5)))
  '((4 7 5 35)))

(test "delayed-*o-9"
  (run* (q)
    (fresh (w x y z)
      (pluso 3 w x)
      (== w 4)
      (== `(,w ,x ,y ,z) q)
      (*o x y z)
      (== y 5)))
  '((4 7 5 35))) 

(test "delayed-*o-10"
  (run* (q)
    (fresh (w x y z)
      (pluso 3 w x)
      (== w 4)
      (== `(,w ,x ,y ,z) q)
      (*o x y z)
      (== y 5)))
  '((4 7 5 35))) 
