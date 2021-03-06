(load "utils.scm")
(load "test-check.scm")


(test "union-1"
  (union '(a b c) '(d e))
  '(a b c d e))

(test "union-2"
  (union '(a b c) '(a d c e))
  '(b a d c e))

(test "union*-1"
  (union* '((a b c) (a d c e)))
  '(b a d c e))

(test "union*-2"
  (union* '((a b c) (a d c e) (a)))
  '(b d c e a))

(test "union*-3"
  (union* '((a b c) (a d c e) (a) () (c f g)))
  '(b d e a c f g))


(test "map-1"
  (map (lambda (x) (list x x)) '(1 2 3))
  '((1 1) (2 2) (3 3)))

(test "map-append-1"
  (map-append (lambda (x) (list x x)) '(1 2 3))
  '(1 1 2 2 3 3))
