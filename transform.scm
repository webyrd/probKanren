(load "mk.scm")
(load "delayed-goal-defs.scm")

; (run 1 (mh proposal prog))

(define prog
  (lambda (q)
    (fresh (x)
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog2
  (lambda (q x)
    (fresh ()
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog2-density
  (lambda (total-density q x)
    (fresh (dx)
      (normal-density x 0.0 1.0 dx)
      (fresh (dq)
        (normal-density q x 1.0 dq)
        (pluso dq dx total-density)))))

(define importance
  (lambda (prog density-value)
    (density prog density-value)))

(run 1 (total-density q x)
  (prog2-density total-density q x)
  (prog2 q x))
