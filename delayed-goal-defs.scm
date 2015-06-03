(load "mk.scm")


;; old version of uniform-sample
; (define uniform-sample
;   (lambda (lo hi)
;     (let ((samp (+ (random (- hi lo)) lo)))
;       samp)))


(define uniform
  (lambda (lo hi x)
    (delayed-goal `(,lo ,hi)
      (project (lo hi x)
        (let ((samp (+ (random (- hi lo)) lo)))
          (== x samp))))))






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
