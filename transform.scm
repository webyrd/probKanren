(load "mk.scm")
(load "mkdefs.scm")
(load "delayed-goal-defs.scm")
(load "utils.scm")
(load "test-check.scm")

;; (define prog
;;   (lambda (q)
;;     (fresh (x)
;;       (normal 0.0 1.0 x)
;;       (normal x 1.0 q))))

;; (define importance
;;   (lambda (prog density-value)
;;     (density prog density-value)))



(define prog3
  (lambda (x b)
    (fresh ()
      (flip 0.6 b)
      (conde
        [(== #t b) (normal 0.0 1.0 x)]
        [(== #f b) (uniform 0.0 1.0 x)]))))

(define prog3-proposal
  (lambda (init-vars new-vars)
    (fresh (b x b^ x^)
      (== (list b x) init-vars)
      (== (list b^ x^) new-vars)
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
             [(== b #f) (uniform 0.0 1.0 x^) (== b b^)])])))))

(define prog3-density
  (lambda (total-density vars)
    (fresh (b x)
      (== (list b x) vars)
      (fresh (db)
        (flip-density b 0.6 db)
        (fresh (dx)
          (conde
            [(== b #t) (normal-density x 0.0 1.0 dx)]
            [(== b #f) (uniform-density x 0.0 1.0 dx)])
          (pluso db dx total-density))))))



(define prog4
  (lambda (q b)
    (fresh ()
      (flip 0.5 b)
      (fresh (x)
        (conde
          [(== #t b)
           (fresh (y)
             (normal 0.0 1.0 x)
             (normal 0.0 1.0 y)
             (== (list x y) q))]
          [(== #f b)
           (uniform 0.0 1.0 x)
           (== (list x) q)])))))



(define prog4-proposal
  (lambda (init-vars new-vars)
    (fresh (b q b^ q^)
      (== (list b q) init-vars)
      (== (list b^ q^) new-vars)
      (fresh (c1)
        (flip 0.5 c1)
        (conde
          [(== #t c1) ;; resample b
           (flip 0.5 b^)
           (conde
             [(== #t b^)
              (fresh (c2 x^ y^)
                (flip 0.5 c2)
                (conde
                  [(== #t c2)
                   (normal 0.0 1.0 x^)
                 
                   (conde
                     [(== #t b)         ; q is length 2
                      (cadro q y^)]
                     [(== #f b)         ; q is length 1
                      (normal 0.0 1.0 y^)])
                 
                   (== (list x^ y^) q^)]
                  [(== #f c2)
                   (normal 0.0 1.0 y^)
                   (caro q x^)
                   (== (list x^ y^) q^)]))]
             [(== #f b^)
              (fresh (x^)
                (uniform 0.0 1.0 x^)
                (== (list x^) q^))])]

          [(== #f c1) ;; resample q
           (== b b^)
           (conde
             [(== #t b)
              (fresh (c2 x^ y^)
                (flip 0.5 c2)
                (conde
                  [(== #t c2)
                   (normal 0.0 1.0 x^)
                   (cadro q y^)
                   (== (list x^ y^) q^)]
                  [(== #f c2)
                   (uniform 0.0 1.0 y^)
                   (caro q x^)
                   (== (list x^ y^) q^)]))]
             [(== #f b)
              (fresh (x^)
                (uniform 0.0 1.0 x^)
                (== (list x^) q^))])])))))

(define prog4-density
  (lambda (total-density vars)
    (fresh (db b q)
      (== (list b q) vars)
      (flip-density b 0.5 db)     
      (fresh (dq dx dy)
	(conde
          [(== b #t)
           (fresh (a ad)
             (== (list a ad) q)
             (normal-density a  0.0 1.0 dx)
             (normal-density ad 0.1 1.0 dy))
	   (pluso dx dy dq)]
          [(== b #f)
           (fresh (a)
             (caro q a)
             (uniform-density a 0.0 1.0 dq))])
        (pluso db dq total-density)))))




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




;;; general definitions of mh and chain

(define mh
  (lambda (init-vars new-vars
      old-density new-density
      proposal
      density)
    (fresh (candidate-vars b log-ratio ratio accept)
      (proposal init-vars candidate-vars)
      (density old-density  init-vars)
      (density new-density  candidate-vars)
      (minuso new-density old-density log-ratio)
      (expo log-ratio ratio)
      (mino 1.0 ratio accept)
      (flip accept b)
      (conde
        [(== b #t) (== candidate-vars new-vars)]
        [(== b #f) (== init-vars new-vars)])
      ;(printg (old-density new-density accept b) "densities and stuff  ")
      )))

(define chain
  (lambda (len init-vars proposal density ls)
    (conde
      [(== 0 len) (== '() ls)]
      [(== 1 len) (== (list init-vars) ls)]
      [(>o len 1 #t)
       (fresh (d old-density new-density new-vars len-1)
         (== `(,init-vars . ,d) ls)
         (mh init-vars new-vars old-density new-density proposal density)
         (minuso len 1 len-1)
         (chain len-1 new-vars proposal density d))])))





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

(test-random "prog3-chain-1"
  (run 1 (ls)
    (fresh (b x)
      (== #f b)
      (== 0.7 x)
      (chain 100 (list b x) prog3-proposal prog3-density ls)))
  '(((#f 0.7) (#t 0.7) (#f 0.7) (#t 0.7) (#t -0.20541516412797461) (#t -0.20541516412797461) (#t 0.5842890873233187) (#t 0.5842890873233187) (#t 0.16630779112558952) (#t 0.16630779112558952) (#t 0.16630779112558952) (#t 0.3489800674372916) (#t -0.1871956249410388) (#t -0.1871956249410388) (#t 0.35181798161632055) (#t -0.30471033204817993) (#t -0.30471033204817993) (#t 0.6712827451870031) (#t 0.6712827451870031) (#t 0.6712827451870031) (#t 0.6712827451870031) (#t 0.6712827451870031) (#t 1.2675315151513893) (#t 1.2675315151513893) (#t -0.551505289960373) (#t 0.6897123058936746) (#t 0.6897123058936746) (#t 0.6897123058936746) (#t 0.07359232951910477) (#t 0.9401377440688459) (#t -0.06392139777307135) (#t -0.06392139777307135) (#t -0.06392139777307135) (#t -0.9964044657352173) (#t 1.3722885630833612) (#t 0.6799471703168047) (#t 0.6799471703168047) (#t 0.2945314289073889) (#t 0.2945314289073889) (#f 0.2945314289073889) (#f 0.2945314289073889) (#t 0.2945314289073889) (#t -1.4519244694282987) (#t -1.4519244694282987) (#t 0.8417988993294282) (#t -1.2801865226740758) (#t -0.5792450899145727) (#t -0.5792450899145727) (#t -0.5792450899145727) (#t -0.5792450899145727) (#t 0.32874598096431945) (#t 0.32874598096431945) (#f 0.32874598096431945) (#f 0.9647563397149821) (#f 0.9647563397149821) (#t 0.9647563397149821) (#t -0.009633727932152917) (#t -0.009633727932152917) (#t -0.009633727932152917) (#t -0.009633727932152917) (#t -0.009633727932152917) (#t -0.009633727932152917) (#t -0.23102144227746724) (#t 1.0810321302733346) (#t 1.0810321302733346) (#t 1.0810321302733346) (#t 1.0810321302733346) (#t -0.39665794361799317) (#t -0.39665794361799317) (#t 1.9335600949385738) (#t 1.9335600949385738) (#t 0.30407841894951904) (#t 0.30407841894951904) (#t 0.30407841894951904) (#t 0.30407841894951904) (#f 0.30407841894951904) (#f 0.9650774357990182) (#f 0.9650774357990182) (#f 0.19012160266334366) (#t 0.19012160266334366) (#f 0.19012160266334366) (#f 0.7860228447072595) (#f 0.7860228447072595) (#f 0.7860228447072595) (#t 0.7860228447072595) (#t 0.6416359599321219) (#t 0.6416359599321219) (#f 0.6416359599321219) (#t 0.6416359599321219) (#t -0.46241823161521123) (#t -0.46241823161521123) (#t -0.3088047266483334) (#t -0.3088047266483334) (#t 1.026489440660579) (#t -0.05361705340829784) (#t -0.8031542074461695) (#t -0.6657681242384056) (#t -0.6657681242384056) (#t -0.6657681242384056) (#t -0.6657681242384056))))

(test-random "prog4-chain-1"
  (run 1 (ls)
    (fresh (b q)
      (== #f b)
      (== (list 0.7) q)
      (chain 12 (list b q) prog4-proposal prog4-density ls)))
  '(((#f (0.7))
     (#f (0.7))
     (#f (0.33237604021746425))
     (#f (0.49624361083488844))
     (#f (0.8177294233977477))
     (#f (0.41107510704973493))
     (#f (0.8762533555943044))
     (#f (0.9344582655547984))
     (#f (0.9344582655547984))
     (#f (0.7661712866286206))
     (#f (0.944030636116415))
     (#f (0.766216211761046)))))
