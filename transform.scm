(load "mk.scm")
(load "delayed-goal-defs.scm")
(load "test-check.scm")

;; (define prog
;;   (lambda (q)
;;     (fresh (x)
;;       (normal 0.0 1.0 x)
;;       (normal x 1.0 q))))

;; (define importance
;;   (lambda (prog density-value)
;;     (density prog density-value)))



;; full worked example:

(define prog2
  (lambda (q x)
    (fresh ()
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog2-proposal
  (lambda (x q x^ q^)
    (fresh (b)
      (flip 0.5 b)
      (conde
        [(== b #t) (normal 0.0 1.0 x^) (== q q^)]
        [(== b #f) (== x x^) (normal x 1.0 q^)]))))

(define prog2-density
  (lambda (total-density q x)
    (fresh (dx)
      (normal-density x 0.0 1.0 dx)
      (fresh (dq)
        (normal-density q x 1.0 dq)
        (pluso dq dx total-density)))))

(define prog2-mh
  (lambda (x q x^^ q^^ density-xq density-xq^)
    (fresh (x^ q^ b ratio accept)
      (prog2-proposal x q x^ q^)
      (prog2-density density-xq  x  q )
      (prog2-density density-xq^ x^ q^)
      (/o density-xq^ density-xq ratio)
      (mino 1.0 ratio accept)
      (flip accept b)
      (conde
        [(== b #t) (== x^ x^^) (== q^ q^^)]
        [(== b #f) (== x  x^^) (== q  q^^)]))))

(define prog2-chain
  (lambda (len x q ls)
    (conde
      [(== 0 len) (== '() ls)]
      [(== 1 len) (== (list (list x q)) ls)]
      [(>o len 1 #t)
       (fresh (d density-xq density-xq^  x^^ q^^ len-1)
         (== `((,x ,q) . ,d) ls)
         (prog2-mh x q x^^ q^^ density-xq density-xq^)
         (minuso len 1 len-1)
         (prog2-chain len-1 x^^ q^^ d))])))

(test-random "prog2-chain-1"
  (run 1 (ls)
    (fresh (x q)
      (== 1.0 x)
      (== 1.2 q)
      (prog2-chain 10 x q ls)))
  '(((1.0 1.2)
     (-0.8755948399394972 1.2)
     (-0.8755948399394972 -2.88744391732116)
     (0.4702758524429722 -2.88744391732116)
     (0.4702758524429722 0.46741683559566044)
     (0.4702758524429722 1.054564939766291)
     (-1.9013778493668034 1.054564939766291)
     (-1.9013778493668034 1.054564939766291)
     (-1.9013778493668034 1.054564939766291)
     (-1.9013778493668034 1.054564939766291))))

(test-random "prog2-density"
  (run 1 (total-density q x)
    (prog2-density total-density q x)
    (prog2 q x))
  '((-2.528524373603396 -1.2269862731156183 -1.1740941342295155)))
