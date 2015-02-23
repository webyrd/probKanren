(define repeato
  (lambda (n f q)
    (cond
      [(= n 0) (== q '())]
      [else
       (fresh (x res)
         (f x)
         (repeato (sub1 n) f res)
         (== (cons x res) q))])))

(define map-goalo_
  (lambda (f ls)
    (conj*-aux (map f ls))))

(define map-goalo
  (lambda (f ls g)
    (conj*-aux (map (lambda (x)
		      (f x q)) ls))))

(define zipwitho
  (lambda (f a b g)
    (cond
     [(null? a) (== g '())]
     [else
      (fresh (x xs)
	(f (car a) (car b) res)
	(zipwitho f (cdr a) (cdr b) xs)
	(== (cons x xs) g))])))
