(load "mk.scm")
(load "mkdefs.scm")
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


;; TODO: Next example to try to hand-compile:
(define prog3
  (lambda (x b)
    (fresh ()
      (flip 0.6 b)
      (conde
        [(== #t b) (normal 0.0 1.0 x)]
        [(== #f b) (uniform 0.0 1.0 x)]))))

(define prog3-proposal
  (lambda (x b x^ b^)
    (fresh (b1)
      (flip 0.5 b1)
      (conde
        [(== b1 #t) ;; b1 is true resample the b in flip
	 (flip 0.6 b^)
	 (conde
           [(== b^ #t) (== x x^)]
           [(== b^ #f) (== x x^)])]
        [(== b1 #f) ;; b1 is false resample whatever x is
	 (conde     ;; so was x normal or uniform?
           [(== b #t) (normal 0.0 1.0 x^) (== b b^)]
           [(== b #f) (uniform 0.0 1.0 x^) (== b b^)])]
        ))))

(define prog3-density
  (lambda (total-density b x)
    (fresh (db)
      (flip-density b 0.6 db)
      (fresh (dx)
	(conde
          [(== b #t) (normal-density x 0.0 1.0 dx)]
          [(== b #f) (uniform-density x 0.0 1.0 dx)])
        (pluso db dx total-density)))))


;; TODO: Another example to try to hand-compile:
(define prog4
  (lambda (q b)
    (fresh ()
      (flip 0.5 b)
      (conde
        [(== #t b)
         (fresh (y)
           (normal 0.0 1.0 x)
           (normal 0.0 1.0 y)
           (== (list x y) q))]
        [(== #f b)
         (uniform 0.0 1.0 x)
         (== (list x) q)]))))

(define prog4-proposal
  (lambda (q b q^ b^)
    (fresh (c1)
      (flip 0.5 c1)
      (conde
       [(== #t c1) ;; resample b
	(flip 0.5 b^) (== q q^)]
       [(== #f c1)
	(== b b^)
	(conde
	 [(== #t b)
	  (fresh (c2 x^ y^)
	    (flip 0.5 c3)
	    (conde
	     [(== #t c2)
	      (normal 0.0 1.0 x^)
	      (== (cadr q) y^)
	      (== (list x^ y^) q^)]
	     [(== #f c2)
	      (uniform 0.0 1.0 y^)
	      (== (car q) x^)
	      (== (list x^ y^) q^)]))]
	 [(== #f b)
	  (fresh (x^)
	    (uniform 0.0 1.0 x^)
	    (== (list x^) q^))])]))))
	    
	  

;; TODO: after trying prog3 and prog4, write the program transformations
      


;; full worked example:

(define prog2
  (lambda (q x)
    (fresh ()
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog2-c
  (lambda (q x)
    (fresh ()
      (== 2.0 x)
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

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
        [(== b #f) (== x x^) (normal x 1.0 q^)])
      )))

(define prog2-proposal-c
  (lambda (x q x^ q^)
    (fresh (b)

      ;; conditioning!
      (== 2.0 x^)

      ;; Super chobo hack, needed only when the conditioning value and
      ;; the initial value are inconsistent:
      (onceo
        (fresh ()
          alwayso
          (flip 0.5 b)
          (conde
            [(== b #t) (normal 0.0 1.0 x^) (== q q^)]
            [(== b #f) (== x x^) (normal x 1.0 q^)])))

      ;; If we insist the conditioning value and the initial value are
      ;; consistent, we can use this code:
      ;;
      ;; (conde
      ;;   [(== b #t) (normal 0.0 1.0 x^) (== q q^)]
      ;;   [(== b #f) (== x x^) (normal x 1.0 q^)])

      )))

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
       (fresh (d density-xq density-xq^ x^^ q^^ len-1)
         (== `((,x ,q) . ,d) ls)
         (prog2-mh x q x^^ q^^ density-xq density-xq^)
         (minuso len 1 len-1)
         (prog2-chain len-1 x^^ q^^ d))])))

(define prog2-density-c
  (lambda (total-density q x)
    (fresh (dx)
      (normal-density x 0.0 1.0 dx)
      (fresh (dq)
        (normal-density q x 1.0 dq)
        (pluso dq dx total-density)))))

(define prog2-mh-c
  (lambda (x q x^^ q^^ density-xq density-xq^)
    (fresh (x^ q^ b ratio accept)
      (prog2-proposal-c x q x^ q^)
      (prog2-density-c density-xq  x  q )
      (prog2-density-c density-xq^ x^ q^)
      (/o density-xq^ density-xq ratio)
      (mino 1.0 ratio accept)
      (flip accept b)
      (conde
        [(== b #t) (== x^ x^^) (== q^ q^^)]
        [(== b #f) (== x  x^^) (== q  q^^)]))))

(define prog2-chain-c
  (lambda (len x q ls)
    (conde
      [(== 0 len) (== '() ls)]
      [(== 1 len) (== (list (list x q)) ls)]
      [(>o len 1 #t)
       (fresh (d density-xq density-xq^ x^^ q^^ len-1)
         (== `((,x ,q) . ,d) ls)
         (prog2-mh-c x q x^^ q^^ density-xq density-xq^)
         (minuso len 1 len-1)
         (prog2-chain-c len-1 x^^ q^^ d))])))


(test-random "prog2-chain-1"
  (run 1 (ls)
    (fresh (x q)
      (== 1.0 x)
      (== 1.2 q)
      (prog2-chain 12 x q ls)))
  '(((1.0 1.2)
     (-0.8755948399394972 1.2)
     (-0.8755948399394972 -2.88744391732116)
     (0.4702758524429722 -2.88744391732116)
     (0.4702758524429722 0.46741683559566044)
     (0.4702758524429722 1.054564939766291)
     (-1.9013778493668034 1.054564939766291)
     (-1.9013778493668034 1.054564939766291)
     (-1.9013778493668034 1.054564939766291)
     (-1.9013778493668034 1.054564939766291)
     (0.08518168064943167 1.054564939766291)
     (0.08518168064943167 1.6422555569972688))))

(test-random "prog2-density"
  (run 1 (total-density q x)
    (prog2-density total-density q x)
    (prog2 q x))
  '((-2.528524373603396 -1.2269862731156183 -1.1740941342295155)))

(test-random "prog2-chain-1-c"
  (run 1 (ls)
    (fresh (x q)
      (== 1.0 x)
      (== 1.2 q)
      (prog2-chain-c 10 x q ls)))
  '(((1.0 1.2)
     (2.0 1.2)
     (2.0 1.2)
     (2.0 1.2)
     (2.0 1.5601195272144106)
     (2.0 1.5601195272144106)
     (2.0 3.648440896939161)
     (2.0 3.648440896939161)
     (2.0 2.8838541058847027)
     (2.0 3.5762912355838488))))

(test-random "prog2-density-c"
  (run 1 (total-density q x)
    (prog2-density-c total-density q x)
    (prog2-c q x))
  '((-4.5271255844254235 0.8259058657704845 2.0)))
