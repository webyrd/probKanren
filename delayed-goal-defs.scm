(load "mk.scm")

;; (define uniform
;;   (lambda (lo hi x)
;;     (delayed-goal `(,lo ,hi)
;;       (project (lo hi x)
;;         (let ((samp (+ (random (- hi lo)) lo)))
;;           (== x samp))))))

(define uniform
  (lambda (lo hi x)
    (delayed-goal `(,lo ,hi)
      (project (lo hi x)
        (if (ground? x)
            succeed
            (let ((samp (+ (random (- hi lo)) lo)))
              (== x samp)))))))

(define uniform-density
  (lambda (x lo hi dx)
    (delayed-goal `(,x ,lo ,hi)
      (project (x lo hi dx)
	(== (if (and (>= x lo) (<= x hi))
		(log (/ (- hi lo)))
		(log 0.0))
	    dx)))))

;; (define flip
;;   (lambda (p x)
;;     (delayed-goal p
;;       (project (p x)
;;         (let ((samp (random 1.0)))
;;           (== (<= samp p) x))))))

(define flip
  (lambda (p x)
    (delayed-goal p
      (project (p x)
        (if (ground? x)
            succeed
            (let ((samp (random 1.0)))
              (== (<= samp p) x)))))))

(define flip-density
  (lambda (x p dx)
    (delayed-goal `(,x ,p)
      (project (x p dx)
	(== (log (if x p (- 1 p)))
	    dx)))))

;; (define normal
;;   (lambda (mu sd x)
;;     (delayed-goal `(,mu ,sd)
;;       (project (mu sd x)
;;         (begin
;;           (when (< sd 0)
;;             (error 'normal "given invalid parameters"))
;;           (== (+ mu (* sd (car (marsaglia)))) x))))))

(define normal
  (lambda (mu sd x)
    (delayed-goal `(,mu ,sd)
      (project (mu sd x)
        (begin
          (when (< sd 0)
            (error 'normal "given invalid parameters"))
          (if (ground? x)
              succeed
              (== (+ mu (* sd (car (marsaglia)))) x)))))))

(define normal-density
  (lambda (x mu sd dx)
    (delayed-goal `(,x ,mu ,sd)
      (project (x mu sd dx)
        (let ((sq (lambda (x) (* x x))))
          (let ((tau (/ (sq sd)))
                (pi 3.141592653589793))
            (== (/ (+ (* (- tau) (sq (- x mu)))
                      (log (/ tau pi 2))) 2)
                dx)))))))


(define uniform-sample
  (lambda (lo hi)
    (let ((samp (+ (random (- hi lo)) lo)))
      samp)))

(define marsaglia
  (lambda ()
    (let ((x (uniform-sample -1.0 1.0))
          (y (uniform-sample -1.0 1.0)))
      (let ((s (+ (* x x) (* y y))))
        (let ((q (sqrt (* -2.0 (log s) (/ s)))))
          (cond
            [(and (>= 1 s) (> s 0))
             (cons (* x q) (* y q))]
            [else (marsaglia)]))))))




(define pluso
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (+ x y) z)))))

(define minuso
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (- x y) z)))))

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

(define mino
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (min x y) z)))))
