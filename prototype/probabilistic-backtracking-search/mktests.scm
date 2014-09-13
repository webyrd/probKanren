(load "mk.scm")
(load "test-check.scm")

(test "1a"
  (run 1 (q) (== q 5))
  '(5))

(test "1b"
  (run 2 (q) (== q 5))
  '(5))

(test "1c"
  (run* (q) (== q 5))
  '(5))

(test "2a"
  (run 1 (q) (== q 5) (== q 6))
  '())

(test "2b"
  (run* (q) (== q 5) (== q 6))
  '())

(test "3a"
  (run 1 (q) (== q 5) (== q 5))
  '(5))

(test "3b"
  (run 2 (q) (== q 5) (== q 5))
  '(5))

(test "3c"
  (run* (q) (== q 5) (== q 5))
  '(5))

(test "4a"
  (run 1 (q)
    (conde
      ((== q 5))
      ((== q 5))))
  '(5))

(test "4b"
  (run 2 (q)
    (conde
      ((== q 5))
      ((== q 5))))
  '(5 5))

(test "4c"
  (run 5 (q)
    (conde
      ((== q 5))
      ((== q 5))))
  '(5 5 5 5 5))

(test "5a"
  (run 1 (q)
    (conde
      ((== q 5))
      ((== q 6))))
  '???)

(test "5b"
  (run 10 (q)
    (conde
      ((== q 5))
      ((== q 6))))
  '???)
