(define repeato
  (lambda (n f q)
    (cond
      [(= n 0) (== q '())]
      [else
       (fresh (x res)
         (f x)
         (repeato (sub1 n) f res)
         (== (cons x res) q))])))

(define seq-goalo
  (lambda (f ls)
    (conde
      ((== '() ls))
      ((fresh (a d)
         (== `(,a . ,d) ls)
         (f a)
         (seq-goalo f d))))))

(define map-goalo
  (lambda (f ls out)
    (conde
      ((== '() ls) (== '() out))
      ((fresh (a d v res)
         (== `(,a . ,d) ls)
         (== `(,v . ,res) out)
         (f x v)
         (map-goalo f d res))))))

(define zipwitho
  (lambda (f l s out)
    (conde
      [(== '() l) (== '() s) (== '() out)]
      [(fresh (al dl as ds v res)
         (== `(,al . ,dl) l)
         (== `(,as . ,ds) s)
         (== `(,v . ,res) out)
         (f al as v)
         (zipwitho f dl ds res))])))



;;; old and busted
#|
(define seq-goalo
  (lambda (f ls)
    (conj*-aux (map f ls))))
|#

#|
(define map-goalo
  (lambda (f ls g)
    (conj*-aux (map (lambda (x)
		      (f x q)) ls))))
|#

#|
(define zipwitho
  (lambda (f a b g)
    (cond
      [(null? a) (== '() g)]
      [else
       (fresh (x xs)
         (f (car a) (car b) res)
         (zipwitho f (cdr a) (cdr b) xs)
         (== (cons x xs) g))])))
|#
