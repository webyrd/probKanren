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
  (let ((val (run 1 (q)
               (conde
                 ((== q 5))
                 ((== q 6))))))
    (printf "val: ~s\n" val)
    (and
      (= (length val) 1)
      (for-all (lambda (v) (or (= 5 v) (= 6 v))) val)))
  #t)

(test "5b"
  (let ((val (run 10 (q)
               (conde
                 ((== q 5))
                 ((== q 6))))))
    (printf "val: ~s\n" val)
    (and
      (= (length val) 10)
      (for-all (lambda (v) (or (= 5 v) (= 6 v))) val)))
  #t)

(test "5c"
  (let ((val (run 100 (q)
               (conde
                 ((== q 5))
                 ((== q 6))))))
    (printf "val: ~s\n" val)
    (and
      (= (length val) 100)
      (for-all (lambda (v) (or (= 5 v) (= 6 v))) val)))
  #t)

(test "9a"
  (let ((val (run 10 (q)
               (conde
                 ((conde
                    ((== q 1))
                    ((== q 2))))
                 ((conde
                    ((== q 3))
                    ((== q 4))))))))
    val)
  '???)

(test "6a"
  (let ((val (run 10 (q)
               (fresh (x y)
                 (conde
                   ((== x 1))
                   ((== x 2)))
                 (conde
                   ((== y 3))
                   ((== y 4)))
                 (== (list x y) q)))))
    val)
  '???)

(test "7a"
  (let ((val (run 10 (q)
               (fresh (x y)
                 (conde
                   ((== x 1))
                   ((== x 2)))
                 (== (list x y) q)
                 (conde
                   ((== y 3))
                   ((== y 4)))))))
    val)
  '???)

(test "8a"
  (let ((val (run 10 (q)
               (fresh (x y)
                 (== (list x y) q)
                 (conde
                   ((== x 1))
                   ((== x 2)))
                 (conde
                   ((== y 3))
                   ((== y 4)))))))
    val)
  '???)

(test "10a"
  (let ((val (run 10 (q)
               (fresh (x)
                 (== x q)
                 (conde
                   ((== x 1))
                   ((== x 2)))))))
    val)
  '???)
