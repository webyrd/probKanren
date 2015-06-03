(load "mk.scm")

(define uniform
  (lambda (lo hi x)
    (delayed-goal `(,lo ,hi)
      (project (lo hi x)
        (let ((samp (+ (random (- hi lo)) lo)))
          (== x samp))))))

(define flip
  (lambda (p x)
    (delayed-goal p
      (project (p x)
        (let ((samp (random 1.0)))
          (== (<= samp p) x))))))




(define pluso
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
