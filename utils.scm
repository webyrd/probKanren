(define mean
  (lambda (ls)
    (exact->inexact (/ (apply + ls) (length ls)))))

(define variance
  (lambda (ls)
    (let ((ls-mean (mean ls))
          (sq      (lambda (x) (* x x))))
      (mean (map (lambda (x) (sq (- x ls-mean))) ls)))))



(define lookup
  (lambda (x ls)
    (cdr (assq x ls))))



(define diff
  (lambda (l1 l2)
    (cond
     [(null? l1) '()]
     [(member (car l1) l2) (diff (cdr l1) l2)]
     [else (cons (car l1) (diff (cdr l1) l2))])))

(define union*
  (lambda (lol)
    (cond
      [(null? lol) '()]
      [(null? (cdr lol)) (car lol)]
      [else (union (car lol) (union* (cdr lol)))])))

(define union
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [(member (car l1) l2) (union (cdr l1) l2)]
      [else (cons (car l1) (union (cdr l1) l2))])))



(define map-append
  (lambda (f ls)
    (cond
      [(null? ls) '()]
      [else (append (f (car ls)) (map-append f (cdr ls)))])))
