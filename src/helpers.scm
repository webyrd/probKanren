(define repeato
  (lambda (n f q)
    (cond
      [(= n 0) (== q '())]
      [else
       (fresh (x res)
         (f x)
         (repeato (sub1 n) f res)
         (== (cons x res) q))])))
(define map-goalo
  (lambda (f ls)
    (conj*-aux (map f ls))))
