(define mean
  (lambda (ls)
    (exact->inexact (/ (apply + ls) (length ls)))))

(define variance
  (lambda (ls)
    (let ((ls-mean (mean ls))
          (sq      (lambda (x) (* x x))))
      (mean (map (lambda (x) (sq (- x ls-mean))) ls)))))
