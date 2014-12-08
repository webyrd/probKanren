(load "mk.scm")
(load "test-check.scm")

(define mean
  (lambda (ls)
    (exact->inexact (/ (apply + ls) (length ls)))))


(define one-or-two
  (lambda (x)
    (conde
      ((== 1 x))
      ((== 2 x)))))


(define geom 
  (lambda (x n)
    (conde
      ((== n x))
      ((geom x (add1 n))))))

(define geom2/3
  (lambda (x n)
    (conde
      ((== n x))
      ((== n x))
      ((geom2/3 x (add1 n))))))

(define geom2/3b
  (lambda (x n)
    (conde
      ((== n x))      
      ((geom2/3b x (add1 n)))
      ((geom2/3b x (add1 n))))))


(run-mh 1 (q) (== #f #f))

(run-mh 2 (q) (== #f #f))

(run-mh 100 (q) (== #f #f))

(run-mh 1 (q)
  (conde
    ((== #f #f))
    ((== #t #t))))

(run-mh 2 (q)
  (conde
    ((== #f #f))
    ((== #t #t))))

(run-mh 1 (q) (== 0.5 q) (flip q #t))

(run-mh 2 (q) (== 0.5 q) (flip q #t))

(run-mh 100 (q) (== 0.5 q) (flip q #t))


(run-mh 1 (q) (fresh (r) (uniform 0.0 0.4 r) (uniform r 1.0 q) (fresh (x) (== x #t) (flip q x))))

(run-mh 2 (q) (fresh (r) (uniform 0.0 0.4 r) (uniform r 1.0 q) (fresh (x) (== x #t) (flip q x))))

(run-mh 100 (q) (fresh (r) (uniform 0.0 0.4 r) (uniform r 1.0 q) (fresh (x) (== x #t) (flip q x))))


(run-mh 100 (q)
  (conde
    ((== q 1))
    ((== q 2))))


#!eof

;; run and run* are now deprecated

;; Example run can be found in comments for the non-deterministic
;; tests below

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
;; val: (5)

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
;; val: (5 6 6 5 6 6 5 5 5 5)

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
;; val: (5 6 6 5 6 6 6 5 5 6 6 5 6 6 6 5 6 5 6 6 6 5 6 6 5 5 6 6 5 6 6 5 5 6 5 6 5 5 6 5 5 5 5 6 5 6 5 6 5 5 5 5 6 5 6 6 6 6 5 5 5 6 6 5 5 5 5 5 5 6 6 5 6 6 5 6 6 5 6 6 6 5 6 5 5 6 5 6 6 5 5 5 6 5 6 6 5 6 6 5)

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
;; val: ((2 3) (2 4) (2 4) (2 3) (2 4) (2 3) (2 4) (2 4) (2 4) (2 3))

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
;; val: ((1 3) (1 3) (1 4) (2 4) (2 3) (2 3) (2 4) (2 4) (1 4) (1 3))

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
;; val: ((1 3) (1 4) (1 3) (2 4) (2 4) (2 4) (2 3) (2 4) (2 3) (1 4))

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
;; val: ((1 4) (2 3) (1 3) (2 4) (1 4) (1 3) (2 3) (1 3) (2 3) (1 3))

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
;; val: ((1 3) (1 3) (1 4) (1 3) (1 3) (1 3) (1 3) (1 4) (1 3) (2 3))

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
;; val: ((2 3) (1 3) (1 3) (2 4) (2 3) (2 4) (2 4) (2 4) (2 3) (1 3))

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
;; val: (2 1 2 2 2 1 1 2 1 2)

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
;; val: (1 2 1 2 1 2 2 1 1 1)

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
;; val: (3 4 2 2 4 3 3 2 2 8 3 4 3 3 8 7 7 6 5 8 7 1 6 1 4 3 4 3 3 1 2 1 1 5 8 7 7 7 8 6 4 3 3 6 7 8 7 5 6 6)

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
;; val: (2 1 5 3 6 1 2 3 2 2 1 5 6 6 4 5 5 4 6 3)

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
;; val: (2 3 2 3 3 3 3 5 1 5 4 4 6 4 5 5 6 4 3 3)

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
;; val: (1 2 5 5 6 5 6 5 6 6 5 6 5 5 5 4 4 1 1 2)


(test "15a"
  (let ((val (run 20 (q)
               (one-or-two q))))
    (printf "val: ~s\n" val)
    (and
     (= (length val) 20)
     (not (not (for-all (lambda (q) (member q '(1 2))) val)))))
  #t)
;; val: (1 1 1 1 1 1 2 2 1 1 2 1 1 2 1 1 2 2 1 1)

(test "16a"
  (let ((val (run 50 (q)
               (geom q 0))))
    (printf "val: ~s\n" val)
    (and
     (= (length val) 50)
     (for-all (lambda (q) (and (integer? q) (>= q 0))) val)))
  #t)
;; val: (0 0 2 0 0 0 0 0 0 0 0 2 4 2 2 1 3 0 0 0 9 6 7 4 3 1 0 1 1 0 1 3 1 1 1 2 1 1 3 1 1 1 1 2 2 2 2 7 5 5)



;;; both should be around 1.0
(mean (apply append (map (lambda (?) (run 1 (q) (geom q 0))) (make-list 10000))))

(mean (run 10000 (q) (geom q 0)))

(mean (run 1000 (q) (geom2/3b q 0)))

(run-mh 1 (q) (== #f #f))

(run-mh 2 (q) (== #f #f))

(run-mh 100 (q) (== #f #f))

(run-mh 1 (q)
  (conde
    ((== #f #f))
    ((== #t #t))))

(run-mh 1 (q) (== 0.5 q) (flip q #t))

(run-mh 2 (q) (== 0.5 q) (flip q #t))

(run-mh 100 (q) (== 0.5 q) (flip q #t))


(run-mh 1 (q)
  (fresh (r)
    (uniform 0.0 0.4 r)
    (uniform r 1.0 q)
    (fresh (x)
       (== x #t)
       (flip q x))))

(run-mh 2 (q)
  (fresh (r)
    (uniform 0.0 0.4 r)
    (uniform r 1.0 q)
    (fresh (x)
      (== x #t)
      (flip q x))))

(run-mh 100 (q)
  (fresh (r)
    (uniform 0.0 0.4 r)
    (uniform r 1.0 q)
      (fresh (x)
	(== x #t)
	(flip q x))))


;; interesting test--what does this mean?
;;
;; (run 1 (q) (flip 0.00001 q) (flip 0.999999 q))
;;
;; vs.
;;
;; (run 1 (q) (flip 0.999999 q) (flip 0.00001 q))
;;
;; Are these equivalent?  Does this make sense?
