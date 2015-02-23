(load "mk.scm")
(load "helpers.scm")
(load "test-check.scm")
;; TODO  update tests to use 'test-random' macro

;; TODO move some of the tests below the end-of-file marker up, and wrap them in 'test-random'


(define +o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (+ x y) z)))))

(define *o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (* x y) z)))))

(define /o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (/ x y) z)))))

(define >o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (> x y) z)))))

(test-random "delayed-*o-11"
  (run-mh 5 (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (+o 3 x y)
      (uniform 0.0 1.0 x)))
  '((0.4530346801796503 3.45303468017965)
    (0.32420990324899246 3.3242099032489927)
    (0.33185997910836695 3.331859979108367)
    (0.04920610982569329 3.0492061098256933)
    (0.06994743325222941 3.0699474332522296)))

(run-mh 10 (q)
  (conde
    ((== q 5))
    ((== q 6))))


(run-mh 10 (q)
  (conde
    ((== q 5))
    ((== q 6) (== q 7))))

(run-mh 10 (q)
  (conde
    ((== q 6) (== q 7))))

(run-mh 10 (q)
  (conde
    ((== q 5))
    ((== q 6)
     (conde
       ((== q 7))
       ((== q 8))))))


(run-mh 10 (q)
  (uniform 0.0 1.0 q))

(run-mh 10 (q)
  (fresh (x r y)
    (== (list x r y) q)
    (uniform 0.0 1.0 r)
    (conde
      ((== x #t) (*o 2.0 r y))
      ((== x #f) (*o 1.0 r y)))))

(run-mh 10 (q)
  (fresh (x r)
    (uniform 0.0 1.0 r)
    (conde
      ((== x #t) (*o 2.0 r q))
      ((== x #f) (*o 1.0 r q)))))


(define pullingo
 (lambda (lazy? strength pulls)
   (conde
     [(== lazy? #t)
      (/o strength 2.0 pulls)]
     [(== lazy? #f)
      (== strength pulls)])))

(define mean
  (lambda (ls)
    (exact->inexact (/ (apply + ls) (length ls)))))

(define variance
  (lambda (ls)
    (let ((ls-mean (mean ls))
          (sq      (lambda (x) (* x x))))
      (mean (map (lambda (x) (sq (- x ls-mean))) ls)))))

(define one-or-two
  (lambda (x)
    (conde
      ((== 1 x))
      ((== 2 x)))))

(define geom-bool
  (lambda (n)
    (fresh (x)
      (flip 0.5 x)
      (conde
        ((== #t x) (== 0 n))
        ((== #f x)
         (fresh (xs)
           (geom-bool xs)
           (project (xs)
             (== (add1 xs) n))))))))

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

(run-mh 100 (q)
  (conde
    ((uniform 0.0 2.0 q))
    ((uniform 5.0 7.0 q))))

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


(test-random "conde-2"
  (run-mh 100 (q)
    (conde
      ((== q 1))
      ((== q 2))))
  '(2 2 1 2 2 1 1 1 2 2 1 2 1 2 2 2 1 2 2 2 1 2 1 2 1 2 1 1 2 1 2 1 2 1 2 1 2 2 2 1 1 1 1 1 1 1 2 2 2 2 1 2 1 2 2 2 1 1 2 2 1 1 2 1 2 2 1 2 1 2 2 2 2 1 2 1 1 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 1 2 1 2 2 2 2 1))


(run-mh 100 (q)
  (uniform 0.0 1.0 q)
  (fresh (x)
    (flip q x)
    (conde
      ((== x #t))
      ((== x #f)))))

;; Uniform-Mixture test
(run-mh 100 (x)
  (flip 0.5 x)
  (fresh (r)
    (== r -1.0)
    (conde
      ((== x #f)
       (normal 0.0 1.0 r))
      ((== x #t)
       (uniform 0.0 1.0 r)))))





(test "delayed-*o-1"
  (run-mh 10 (x)
    (*o 3 4 x))
  '(12 12 12 12 12 12 12 12 12 12))

(test "delayed-*o-2"
  (run-mh 10 (q)
    (fresh (x y)
      (== x 4)
      (*o 3 x y)
      (== `(,x ,y) q)))
  '((4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12)))

(test "delayed-*o-3"
  (run-mh 10 (q)
    (fresh (x y)
      (*o 3 x y)
      (== x 4)
      (== `(,x ,y) q)))
  '((4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12)))

(test "delayed-*o-4"
  (run-mh 10 (q)
    (fresh (x y)
      (*o 3 x y)
      (== x 4)
      solve-delayed-goals
      (== `(,x ,y) q)))
  '((4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12)))

(test "delayed-*o-5"
  (run-mh 10 (q)
    (fresh (x y)
      (*o 3 x y)
      solve-delayed-goals
      (== x 4)
      (== `(,x ,y) q)))
  '((4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12)))

(test "delayed-*o-6"
  (run-mh 10 (q)
    (fresh (x y)
      solve-delayed-goals
      (*o 3 x y)
      solve-delayed-goals
      (== x 4)
      solve-delayed-goals
      (== `(,x ,y) q)
      solve-delayed-goals))
  '((4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12) (4 12))) 

(test "delayed-*o-7"
  (run-mh 5 (q)
    (fresh (w x y z)
      solve-delayed-goals
      (*o x y z)
      solve-delayed-goals
      (+o 3 w x)
      solve-delayed-goals      
      (== w 4)
      solve-delayed-goals
      (== `(,w ,x ,y ,z) q)
      solve-delayed-goals
      (== y 5)
      solve-delayed-goals))
  '((4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35))) 

(test "delayed-*o-8"
  (run-mh 5 (q)
    (fresh (w x y z)
      solve-delayed-goals
      (+o 3 w x)
      solve-delayed-goals      
      (== w 4)
      solve-delayed-goals
      (== `(,w ,x ,y ,z) q)
      solve-delayed-goals
      (*o x y z)      
      solve-delayed-goals
      (== y 5)
      solve-delayed-goals))
  '((4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35))) 

(test "delayed-*o-9"
  (run-mh 5 (q)
    (fresh (w x y z)
      (+o 3 w x)
      (== w 4)
      (== `(,w ,x ,y ,z) q)
      (*o x y z)
      (== y 5)))
  '((4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35))) 

(test "delayed-*o-10"
  (run-mh 5 (q)
    (fresh (w x y z)
      (+o 3 w x)
      (== w 4)
      (== `(,w ,x ,y ,z) q)
      (*o x y z)
      (== y 5)
      solve-delayed-goals))
  '((4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35) (4 7 5 35))) 



(define pullingo
  (lambda (lazy? strength pulls)
    (conde
      [(== lazy? #t)
       (/o strength 2 pulls)]
      [(== lazy? #f)
       (== strength pulls)])))

(define tug-of-war
  (lambda (samples)
    (run-mh samples (q)
      (fresh (bob sue bob-lazy?1 sue-lazy?1 bob-lazy?2 sue-lazy?2 bob-lazy?3 sue-lazy?3)
	(normal 0.0 1.0 bob)
	(normal 0.0 1.0 sue)

	(flip 0.25 bob-lazy?1)
	(flip 0.25 sue-lazy?1)
	(flip 0.25 bob-lazy?2)
	(flip 0.25 bob-lazy?2)
	(flip 0.25 sue-lazy?3)
	(flip 0.25 sue-lazy?3)

		   (fresh (bob-pulls1 sue-pulls1 bob-pulls2 sue-pulls2 bob-pulls3 sue-pulls3)

			  (pullingo bob-lazy?1 bob bob-pulls1)
			  (pullingo bob-lazy?2 bob bob-pulls2)
			  (pullingo bob-lazy?3 bob bob-pulls3)

			  (pullingo sue-lazy?1 sue sue-pulls1)
			  (pullingo sue-lazy?2 sue sue-pulls2)
			  (pullingo sue-lazy?3 sue sue-pulls3)
			  
			  (fresh (sue-wins1 sue-wins2 sue-wins3)
				 (>o sue-pulls1 bob-pulls1 sue-wins1)
				 (== sue-wins1 #t)

				 (>o sue-pulls2 bob-pulls2 sue-wins2)
				 (== sue-wins2 #t)

				 (>o sue-pulls3 bob-pulls3 sue-wins3)
				 (== sue-wins3 #t)))

		   (== q (list bob sue))))))

(define tug-of-war2
  (lambda (samples)
    (run-mh samples (q)
      (fresh (bob sue)
	(normal 0.0 1.0 bob)
	(normal 0.0 1.0 sue)
	
	(fresh (bob-lazy sue-lazy)
	  (repeato 3 (lambda (x) (flip 0.25 x)) bob-lazy)
	  (repeato 3 (lambda (x) (flip 0.25 x)) sue-lazy)
           
          (fresh (bob-pulls sue-pulls)
            (map-goalo (lambda (x g)
			 (pullingo x bob g)) bob-lazy bob-pulls)
	    (map-goalo (lambda (x g)
			 (pullingo x sue g)) sue-lazy sue-pulls)

	    (fresh (sue-wins)
              (zipwitho >o sue-pulls bob-pulls sue-wins)
	      (seq-goalo (lambda (x) (== x #t)) sue-wins))))
	(== q (list bob sue))))))

;; Example run-mh can be found in comments for the non-deterministic
;; tests below

(test "1a"
  (run-mh 1 (q) (== q 5))
  '(5))

(test "1b"
  (run-mh 2 (q) (== q 5))
  '(5 5))

(test "2a"
  (run-mh 1 (q) (== q 5) (== q 6))
  '())

(test "2b"
  (run-mh 1 (q) (== q 5) (== q 6))
  '())

(test "3a"
  (run-mh 1 (q) (== q 5) (== q 5))
  '(5))

(test "3b"
  (run-mh 2 (q) (== q 5) (== q 5))
  '(5 5))

(test "3c"
  (run-mh 1 (q) (== q 5) (== q 5))
  '(5))

(test "4a"
  (run-mh 1 (q)
    (conde
      ((== q 5))
      ((== q 5))))
  '(5))

(test "4b"
  (run-mh 2 (q)
    (conde
      ((== q 5))
      ((== q 5))))
  '(5 5))

(test "4c"
  (run-mh 5 (q)
    (conde
      ((== q 5))
      ((== q 5))))
  '(5 5 5 5 5))

(test "5a"
  (let ((val (run-mh 1 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 100 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 10 (q)
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
  (let ((val (run-mh 50 (q)
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
  (let ((val (run-mh 20 (q)
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
  (let ((val (run-mh 20 (q)
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
  (let ((val (run-mh 20 (q)
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
  (let ((val (run-mh 20 (q)
               (one-or-two q))))
    (printf "val: ~s\n" val)
    (and
     (= (length val) 20)
     (not (not (for-all (lambda (q) (member q '(1 2))) val)))))
  #t)
;; val: (1 1 1 1 1 1 2 2 1 1 2 1 1 2 1 1 2 2 1 1)

(test "16a"
  (let ((val (run-mh 50 (q)
               (geom q 0))))
    (printf "val: ~s\n" val)
    (and
     (= (length val) 50)
     (for-all (lambda (q) (and (integer? q) (>= q 0))) val)))
  #t)
;; val: (0 0 2 0 0 0 0 0 0 0 0 2 4 2 2 1 3 0 0 0 9 6 7 4 3 1 0 1 1 0 1 3 1 1 1 2 1 1 3 1 1 1 1 2 2 2 2 7 5 5)



;;; both should be around 1.0
(mean (apply append (map (lambda (?) (run-mh 1 (q) (geom q 0))) (make-list 10000))))

(mean (run-mh 10000 (q) (geom q 0)))

(mean (run-mh 1000 (q) (geom2/3b q 0)))

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

(run-mh 100 (q)
  (uniform 0.0 1.0 q))

(mean
  (run-mh 1000 (q)
    (uniform 0.0 1.0 q)
    (flip q #t)))
;; should produce a value near 2/3

(mean
  (run-mh 1000 (q)
    (uniform 0.0 1.0 q)
    (flip q #f)))
;; should produce a value near 1/3

(mean
  (run-mh 1000 (q)
    (uniform 0.0 1.0 q)
    (fresh (x)
      (== x #t)
      (flip q x))))
;; should produce a value near 2/3

(mean
  (run-mh 1000 (q)
    (uniform 0.0 1.0 q)
    (fresh (x)
      (flip q x)
      (== x #t))))
;; should produce a value near 2/3

(mean (run-mh 10000 (q) (one-or-two q)))
;; should return an answer near 1.5

(mean (run-mh 10000 (q) (geom q 0)))
;; should return an answer near 1.0

(mean (run-mh 10000 (q) (geom-bool q)))
;; should return an answer near 1.0

(define dice
  (lambda (samples)
    (run-mh samples (q)
      (categorical '[(1 . 1)
		     (2 . 1)
		     (3 . 1)
		     (4 . 1)
		     (5 . 1)
		     (6 . 1)] q))))

(define biased-conde
  (lambda (samples)
    (run-mh samples (q)
      (fresh (r)
        (categorical '[(a . 5)
    		       (b . 1)
		       (c . 3)] r)
        (conde
          [(== r 'a) (== q 2)]
	  [(== r 'b) (== q 5)]
	  [(== r 'c) (== q 7)])))))

(define drop-n
  (lambda (n ls)
    (cond
      [(null? ls) '()]
      [(zero? n) ls]
      [else (drop-n (sub1 n) (cdr ls))])))

(define thin
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null? (cdr ls)) (car ls)]
      [else (cons (car ls) (thin (cddr ls)))])))


;; interesting test--what does this mean?
;;
;; (run 1 (q) (flip 0.00001 q) (flip 0.999999 q))
;;
;; vs.
;;
;; (run 1 (q) (flip 0.999999 q) (flip 0.00001 q))
;;
;; Are these equivalent?  Does this make sense?
