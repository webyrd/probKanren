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
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (x/y) (let ((x (car x/y)) (y (cadr x/y))) (and (member x '(1 2)) (member y '(3 4))))) val)))))
  #t)

(test "6b"
  (let ((val (run 10 (q)
               (fresh (x y)
                 (conde
                   ((== x 1))
                   ((== x 2)))
                 (== (list x y) q)
                 (conde
                   ((== y 3))
                   ((== y 4)))))))
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (x/y) (let ((x (car x/y)) (y (cadr x/y))) (and (member x '(1 2)) (member y '(3 4))))) val)))))
  #t)

(test "6c"
  (let ((val (run 10 (q)
               (fresh (x y)
                 (== (list x y) q)
                 (conde
                   ((== x 1))
                   ((== x 2)))
                 (conde
                   ((== y 3))
                   ((== y 4)))))))
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (x/y) (let ((x (car x/y)) (y (cadr x/y))) (and (member x '(1 2)) (member y '(3 4))))) val)))))
  #t)

(test "6d"
  (let ((val (run 10 (q)
               (fresh (x y)
                 (== (list x y) q)
                 (conde
                   ((== y 3))
                   ((== y 4)))
                 (conde
                   ((== x 1))
                   ((== x 2)))))))
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (x/y) (let ((x (car x/y)) (y (cadr x/y))) (and (member x '(1 2)) (member y '(3 4))))) val)))))
  #t)

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
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (x/y) (let ((x (car x/y)) (y (cadr x/y))) (and (member x '(1 2)) (member y '(3 4))))) val)))))
  #t)

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
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (x/y) (let ((x (car x/y)) (y (cadr x/y))) (and (member x '(1 2)) (member y '(3 4))))) val)))))
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
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (q) (member q '(1 2 3 4))) val)))))
  #t)

(test "10a"
  (let ((val (run 10 (q)
               (fresh (x)
                 (== x q)
                 (conde
                   ((== x 1))
                   ((== x 2)))))))
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 10)
               (for-all (lambda (q) (member q '(1 2))) val)))))
  #t)

(test "11a"
  (let ((val (run 50 (q)
               (conde
                 ((conde
                    ((conde
                       ((== q 1))
                       ((== q 2))))
                    ((conde
                       ((== q 3))
                       ((== q 4))))))
                 ((conde
                    ((conde
                       ((== q 5))
                       ((== q 6))))
                    ((conde
                       ((== q 7))
                       ((== q 8))))))))))
    (printf "val: ~s\n" val)
    (not (not (and
               (= (length val) 50)
               (for-all (lambda (q) (member q '(1 2 3 4 5 6 7 8))) val)))))
  #t)

(test "12a"
  (let ((val (run 20 (q)
               (conde
                 ((== q 1))
                 ((== q 2))
                 ((== q 3))
                 ((== q 4))
                 ((== q 5))
                 ((== q 6))))))
    (printf "val: ~s\n" val)
    (and
      (= (length val) 20)
      (not (not (for-all (lambda (q) (member q '(1 2 3 4 5 6))) val)))))
  #t)

(test "13a"
  (let ((val (run 20 (q)
               (conde
                 ((conde
                    ((== q 1))
                    ((== q 2))
                    ((== q 3))))
                 ((conde
                    ((== q 4))
                    ((== q 5))
                    ((== q 6))))))))
    (printf "val: ~s\n" val)
    (and
     (= (length val) 20)
     (not (not (for-all (lambda (q) (member q '(1 2 3 4 5 6))) val)))))
  #t)

(test "14a"
  (let ((val (run 20 (q)
               (conde
                 ((conde
                    ((== q 1))
                    ((== q 2))))
                 ((conde                    
                    ((== q 3))
                    ((== q 4))))
                 ((conde                    
                    ((== q 5))
                    ((== q 6))))))))
    (printf "val: ~s\n" val)
    (and
     (= (length val) 20)
     (not (not (for-all (lambda (q) (member q '(1 2 3 4 5 6))) val)))))
  #t)

(define one-or-two
  (lambda (x)
    (conde
      ((== 1 x))
      ((== 2 x)))))

(test "15a"
  (let ((val (run 20 (q)
               (one-or-two q))))
    (printf "val: ~s\n" val)
    (and
     (= (length val) 20)
     (not (not (for-all (lambda (q) (member q '(1 2))) val)))))
  #t)
