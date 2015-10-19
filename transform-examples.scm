(load "transform.scm")
(load "mk.scm")
(load "mkdefs.scm")
(load "delayed-goal-defs.scm")
(load "utils.scm")
(load "test-check.scm")

;;; Working hypothesis:
;;;
;;; For density relations, only lift variables that are in the
;;; 'output' position of an rp.




;;; The 'prog-double-x' example shows what happens when the same
;;; variable ('x') appears in the last position in multiple rp's

;; This program could mean (normal 0.0 1.0) or (normal 0.0 2.0)
;; The implementation will pick for you
;;
(define prog-double-x
  (lambda ()
    (fresh (x)
      (normal 0.0 1.0 x)
      (normal 0.0 2.0 x))))

(define prog-double-x-var-lifted
  (lambda (x1)
    (fresh ()
      (normal 0.0 1.0 x1)
      (normal 0.0 2.0 x1))))

(define prog-double-x-proposal
  (lambda (init-vars new-vars)
    (fresh (x1
            x1^)
      (== (list x1) init-vars)
      (== (list x1^) new-vars)
      (fresh (c2)
        ; faking (discrete-uniform 2 c2)
        ; which is equivalent to (categorical c2 '((1.0 0) (1.0 1)))
        (uniform 0 2 c2)
        (conde
          [(== 0 c2)
           (fresh ()
             (normal 0.0 1.0 x1^)
             ;; (== x1 x1^)   better not perform this unification!
             ;; if resampling x1^, better not unify x1^ with x1!
             )]
          [(== 1 c2)
           (fresh ()
             ;; (== x1 x1^)   better not perform this unification!
             ;; if resampling x1^, better not unify x1^ with x1!
             (normal 0.0 2.0 x1^))])))))

(define prog-double-x-density
  (lambda (total-density vars)
    (fresh (x1)
      (== (list x1) vars)
      (fresh (dx1 dx2)
        (fresh ()
          (normal-density 0.0 1.0 x1 dx1)
          (normal-density 0.0 2.0 x1 dx2))
        (sumo (list dx1 dx2) total-density)))))




;;; The 'prog0' examples demonstrates more than two rp's, and a non-rp
;;; goal.
(define prog0
  (lambda (q)
    (fresh (x y z)
      (normal 0.0 1.0 x)
      (normal x 1.0 y)
      (pluso x y z)
      (normal z 1.0 q))))

(define prog0-var-lifted
  (lambda (x1 y2 q3)
    (fresh (z)
      (normal 0.0 1.0 x1)
      (normal x1 1.0 y2)
      (pluso x1 y2 z)
      (normal z 1.0 q3))))

(define prog0-proposal
  (lambda (init-vars new-vars)
    (fresh (x1 y2 q3
            x1^ y2^ q3^)
      (== (list x1 y2 q3) init-vars)
      (== (list x1^ y2^ q3^) new-vars)

      (fresh (choice4)
        ;; There are three init variables: x1 y2 q3
        ;; We randomly chose one of these variables
        ;; to resample.
        ;; faking (discrete-uniform 3 c4)
        ;; which is equivalent to (categorical c4 '((1.0 0) (1.0 1) (1.0 2)))        
        (uniform 0 3 choice4)
        (conde
          [(== 0 c4) ; here we resample x1
           (fresh (z)
             (normal 0.0 1.0 x1^)
             (== y2 y2^)
	     (normal x1 1.0 y2)
             (pluso x1^ y2^ z)
             (== q3 q3^)
	     (normal z 1.0 q3))]          
          [(== 1 choice4) ; here we resample y2
           (fresh (z)
             (== x1 x1^)
	     (normal 0.0 1.0 x1)
             (normal x1^ 1.0 y2^)
             (pluso x1^ y2^ z)
             (== q3 q3^)
	     (normal z 1.0 q3))]
          [(== 2 choice4) ; here we resample q3
           (fresh (z)
             (== x1 x1^)
	     (normal 0.0 1.0 x1)
             (== y2 y2^)
	     (normal x1 1.0 y2)
             (pluso x1^ y2^ z)
             (normal z 1.0 q3^))])))))

(define prog0-density
  (lambda (total-density vars)
    (fresh (x1 y2 q3)
      (== (list x1 y2 q3) vars)
      (fresh (dx1 dy2 dq3)
        (fresh (z)
          (normal-density 0.0 1.0 x1 dx1)
          (normal-density x1 1.0 y2 dy2)
          (pluso x1 y2 z)
          (normal-density z 1.0 q3 dq3))
        (sumo (list dx1 dy2 dq3) total-density)))))




;;;

(define prog1
  (lambda (q)
    (fresh (x)
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog1-var-lifted
  (lambda (x1 q2)
    (fresh ()
      (normal 0.0 1.0 x1)
      (normal x1 1.0 q2))))

(define prog1-proposal
  (lambda (init-vars new-vars)
    (fresh (x1 q2 x1^ q2^)  ;; 'x' has been lifted to top level
      (== (list x1 q2) init-vars)
      (== (list x1^ q2^) new-vars)

      (fresh (choice3)
        (uniform 0 2 choice3)
        (conde
          [(== 0 choice3)
           (fresh ()
             (normal 0.0 1.0 x1^)
             (== q2 q2^)
	     (normal x1 1.0 q2))]
          [(== 1 choice3)
           (fresh ()
             (== x1 x1^)
	     (normal 0.0 1.0 x1)
             (normal x1 1.0 q2^))])))))

(define prog1-density
  (lambda (total-density vars)
    (fresh (x q) ;; 'x' has been lifted to top level, so we can calculate density
      (== (list x q) vars)
      (fresh (dx dq)
        ;;  original program body, with rp's changed to density relations and 'x' lifted
        (fresh () ;; pulled the 'x' out of this fresh
          (normal-density 0.0 1.0 x dx)
          (normal-density x 1.0 q dq))
        ;;
        (sumo (list dx dq) total-density)))))


;;;

(define prog2
  (lambda (x q)
    (fresh ()
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog2-var-lifted
  (lambda (x q)
    (fresh ()
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog2-var-lifted-proposal
  (lambda (init-vars new-vars)
    (fresh (x q x^ q^)
      (== (list x q) init-vars)
      (== (list x^ q^) new-vars)
      (fresh (choice)
        (uniform 0 2 choice)
        (conde
          ((== 0 choice)
           (normal 0.0 1.0 x^)
           (== q q^)
           (normal x 1.0 q))
          ((== 1 choice)
           (== x x^)
           (normal 0.0 1.0 x)
           (normal x 1.0 q^)))))))

(define prog2-density
  (lambda (total-density vars)
    (fresh (x q)
      (== (list x q) vars)
      (fresh (dx dq)
        ;; original program body, with rp's changed to density relations
        (fresh ()
          (normal-density 0.0 1.0 x dx)
          (normal-density x 1.0 q dq))
        ;;
        (sumo (list dx dq) total-density)))))



;;; conditioning version of prog2

(define prog2-c
  (lambda (q x)
    (fresh ()
      (== 2.0 x)
      (normal 0.0 1.0 x)
      (normal x 1.0 q))))

(define prog2-proposal-c
  (lambda (init-vars new-vars)
    (fresh (x q x^ q^)
      (== (list x q) init-vars)
      (== (list x^ q^) new-vars)
      (fresh (choice)

        ;; conditioning!
        (== 2.0 x^)

        (uniform 0 2 choice)
        (conde
          [(== 0 choice) (normal 0.0 1.0 x^) (== q q^)]
          [(== 1 choice) (== x x^) (normal x 1.0 q^)])))))


;;;

(define prog3
  (lambda (b x)
    (fresh ()
      (flip 0.6 b)
      (conde
        [(== #t b) (normal 0.0 1.0 x)]
        [(== #f b) (uniform 0.0 1.0 x)]))))

(define prog3-var-lifted
  (lambda (b x)
    (fresh
      ()
      (flip 0.6 b)
      (conde
        ((== #t b) (normal 0.0 1.0 x))
        ((== #f b) (uniform 0.0 1.0 x))))))

(define prog3-proposal
  (lambda (init-vars new-vars)
    (fresh (b x b^ x^)
      (== (list b x) init-vars)
      (== (list b^ x^) new-vars)

      (fresh (choice)
        (uniform 0 2 choice)
        (conde
          [(== 0 choice) ;; resample the b in flip
           (flip 0.6 b^)
           (conde
             [(== b^ #t)
	      (== x x^)
	      (normal 0.0 1.0 x)]
             [(== b^ #f)
	      (== x x^)
	      (uniform 0.0 1.0 x)])]
          [(== 1 choice) ;; resample whatever x is
           (conde     ;; so was x normal or uniform?
             [(== b #t)
	      (normal 0.0 1.0 x^)
	      (== b b^)
	      (flip 0.6 b)]
             [(== b #f)
	      (uniform 0.0 1.0 x^)
	      (== b b^)
	      (flip 0.6 b)])])))))

(define prog3-density
  (lambda (total-density vars)
    (fresh (b x)
      (== (list b x) vars)
      (fresh (db dx)
        ;; original program body, with rp's changed to density relations
        (fresh ()
          (flip-density 0.6 b db)
          (conde
            [(== #t b) (normal-density 0.0 1.0 x dx)]
            [(== #f b) (uniform-density 0.0 1.0 x dx)]))
        ;;
        (sumo (list db dx) total-density)))))

;;;

;;; The 'prog4' example test random variables that appear in only one branch of the conde.
(define prog4
  (lambda (b q)
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

(define prog4-lifted
  (lambda (b q x y)
    (fresh ()
      (flip 0.5 b)
      (fresh () ; lift x
        (conde
          [(== #t b)
           (fresh () ; lift y
             (normal 0.0 1.0 x)
             (normal 0.0 1.0 y)
             (== (list x y) q))]
          [(== #f b)
           (uniform 0.0 1.0 x)
           (== (list x) q)])))))

(define prog4-proposal
  (lambda (init-vars new-vars)
    (fresh (b q x y
            b^ q^ x^ y^)
      (== (list b q x y) init-vars)
      (== (list b^ q^ x^ y^) new-vars)

      (fresh (choice)
        (uniform 0 4 choice)
        (conde
          [(== 0 choice) ; here we resample b
           (flip 0.5 b^)
           (fresh ()
             (conde
               [(== #t b^)
                (fresh ()

                  (== x x^)
		  (normal 0.0 1.0 x)
                  
                  (== y y^)
		  (normal 0.0 1.0 y)
                  
		  (== (list x^ y^) q^))]
               [(== #f b^)

                (== x x^)
		(uniform 0.0 1.0 x)

                (== (list x^) q^)]))]
          [(== 1 choice) ; here we resample q
	   (fresh ()
	     (== b b^)
	     (flip 0.5 b)
	     (fresh () ; lift x
	       (conde
		[(== #t b^)
		 (fresh () ; lift y
		   (== x x^)
		   (normal 0.0 1.0 x)
		   (== y y^)
		   (normal 0.0 1.0 y)
		   (== (list x^ y^) q^))]
		[(== #f b^)
		 (== x x^)
		 (uniform 0.0 1.0 x)
		 (== (list x^) q^)])))
           ]
          [(== 2 choice) ; here we resample x
	   (fresh ()
	     (== b b^)
	     (flip 0.5 b)
	     (fresh () ; lift x
	       (conde
		[(== #t b^)
		 (fresh () ; lift y
		   (normal 0.0 1.0 x^)
		   (== y y^)
		   (normal 0.0 1.0 y)
		   (== (list x^ y^) q^))]
		[(== #f b^)
		 (== x x^)
		 (uniform 0.0 1.0 x)
		 (== (list x^) q^)])))
           ]
          [(== 3 choice) ; here we resample y
	   (fresh ()
	     (== b b^)
	     (flip 0.5 b)
	     (fresh () ; lift x
	       (conde
		[(== #t b^)
		 (fresh () ; lift y
		   (== x x^)
		   (normal 0.0 1.0 x)
		   (normal 0.0 1.0 y^)
		   (== (list x^ y^) q^))]
		[(== #f b^)
		 (== x x^)
		 (uniform 0.0 1.0 x)
		 (== (list x^) q)])))
           ])))))







(define prog4-proposal
  (lambda (init-vars new-vars)
    (fresh (b q x y
            b^ q^ x^ y^)
      (== (list b q x y) init-vars)
      (== (list b^ q^ x^ y^) new-vars)
      (fresh (c1)
        (flip 0.5 c1)
        (conde
          [(== #t c1) ;; resample b
           (flip 0.5 b^)
           (conde
             [(== #t b^)
              (fresh (c2)
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
              (fresh ()
                (uniform 0.0 1.0 x^)
                (== (list x^) q^))])]

          [(== #f c1) ;; resample q
           (== b b^)
           (conde
             [(== #t b)
              (fresh (c2)
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
              (fresh ()
                (uniform 0.0 1.0 x^)
                (== (list x^) q^))])])))))

(define prog4-density
  (lambda (total-density vars)
    (fresh (b q x y)
      (== (list b q x y) vars)
      (fresh (db dq dx dy)
        ;;
        (fresh ()
          (flip-density 0.5 b db)
          (fresh () ;; lift 'x'
            (conde
              [(== #t b)
               (fresh () ;; lift 'y'
                 (normal-density 0.0 1.0 x dx)
                 (normal-density 0.0 1.0 y dy)
                 (== (list x y) q))]
              [(== #f b)
               (uniform-density 0.0 1.0 x dx)
               (== (list x) q)
               ;;
               (== 0.0 dy) ;; since 'dy' isn't involved in this clause
               ;;
               ])))
        ;;
        (sumo (list db dx dy) total-density)))))

;;;

(define geom
  (lambda (p q)
    (fresh (b)
      (flip p b)
      (conde
        [(== #t b)
         (== 0 q)]
        [(== #f b)
         (fresh (res)
           (geom p res)
           (pluso 1 res q))]))))

(define geom-var-lifted
  (lambda (p q b res)
    (fresh ()
      (flip p b)
      (conde
        ((== #t b) (== 0 q))
        ((== #f b)
          (fresh (b$ res$)
            (geom-var-lifted p res b$ res$))
          (pluso 1 res q))))))

(define geom-proposal
  ;; geom-proposal needs to look at the b's from the trace
  (lambda (init-vars new-vars)
    (fresh (q b res p
            q^ b^ res^ p^)
      (== (list q b res p) init-vars)
      (== (list q^ b^ res^ p^) new-vars)
      (fresh (choice)
	(uniform 0 4 choice)
	(conde
	 [(== 0 choice)
	  (fresh ()
	    (== b b^)
            (flip p b)
            (conde
              [(== #t b^)
	       (== 0 q^)]
              [(== #f b^)
               (fresh (x y)
		 (geom-var-lifted p res x y))
	       (== res res^)
	       (pluso 1 res^ q^)]))]
	 [(== 1 choice)
	  (fresh ()
            (flip p^ b^)
	    (conde
              [(== #t b^)
	       (== 0 q^)]
              [(== #f b^)
	       (== res res^)
	       (fresh (x y)
		 (geom-var-lifted p res x y))
	       (pluso 1 res^ q^)]))]
	 [(== 2 choice)
	  (fresh ()
	    (== b b^)
	    (flip p b)
	    (conde
             [(== #t b^)
	      (== 0 q^)]
	     [(== #f b^)
	       (fresh (x y)
		(geom-var-lifted p res x y))
	       (fresh (x y)
	        (geom-var-lifted p^ res^ x y))
	      (pluso 1 res^ q^)]))]
	 [(== 3 choice)
	  (fresh ()
	   (== b b^)
           (flip p b)
	   (conde
	    [(== #t b^)
	     (== 0 q^)]
	    [(== #f b^)
	     (== res res^)
	     (fresh (x y)
	       (geom-var-lifted p res x y))
	     (pluso 1 res^ q)]))])))))



(define geom-density
  (lambda (total-density vars)
    (fresh (p q b b*) ; have a list of b's (b . b*) from the trace...
      (== (list p q (cons b b*)) vars)
      (fresh (db total-density-res)
        ;;  original program body, with rp's changed to density relations
        (fresh () ;; lifted 'b'
          (flip-density p b db)
          (conde
            [(== #t b)
             (== '() b*)
             (== 0 q)

             ;;
             (== 0.0 total-density-res)
             ;;
             
             ]
            [(== #f b)
             (fresh (res) ;; don't lift 'res', since it isn't from an rp
               
               ;; original call: (geom p res)
               (geom-density total-density-res (list p res b*))
               ;;
               
               (pluso 1 res q))]))
        (sumo (list db total-density-res) total-density)))))




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
        [(== b #f) (== init-vars new-vars)]))))

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



;;; TODO: implement importance sampler
(define importance
  (lambda (prog density-value)
    (density prog density-value)))



;;; tests

(test-random "prog-double-x-density"
  (run* (total-density)
    (prog-double-x-density total-density (list 0.5)))
  '(-2.6872742469692907))

(test-random "prog-double-x-proposal"
  (run 1 (q)
    (prog-double-x-proposal '(0.5) q))
  '((-3.4245136183523086)))

(test-random "prog-double-x-chain-1"
  (run 1 (ls)
    (fresh (x)
      (== 1.0 x)
      (chain 12 (list x) prog-double-x-proposal prog-double-x-density ls)))
  '(((1.0)
     (1.0)
     (1.0)
     (1.0)
     (-0.879760945571179)
     (-0.8178679492620285)
     (-0.8178679492620285)
     (-0.522572176269937)
     (-0.522572176269937)
     (-0.38922204676677685)
     (-0.38922204676677685)
     (-0.38922204676677685))))


(test-random "prog1-chain-1"
  (run 1 (ls)
    (fresh (x q)
      (== 1.0 x)
      (== 1.2 q)
      (chain 12 (list x q) prog1-proposal prog1-density ls)))
  '(((1.0 1.2) (-0.8755948399394972 1.2) (-0.8755948399394972 -2.88744391732116) (-0.8755948399394972 -2.88744391732116) (-0.8755948399394972 -0.8784538567868089) (-0.8755948399394972 -0.2913057526161784) (-0.8755948399394972 -0.2913057526161784) (-0.8755948399394972 -1.4966988999433553) (-0.8755948399394972 0.00825926594520543) (-0.8755948399394972 -0.2940985014512921) (0.08518168064943167 -0.2940985014512921) (0.08518168064943167 -0.2940985014512921))))



(test-random "prog2-chain-1"
  (run 1 (ls)
    (fresh (x q)
      (== 1.0 x)
      (== 1.2 q)
      (chain 12 (list x q) prog2-proposal prog2-density ls)))
  '(((1.0 1.2) (-0.8755948399394972 1.2) (-0.8755948399394972 -2.88744391732116) (-0.8755948399394972 -2.88744391732116) (-0.8755948399394972 -0.8784538567868089) (-0.8755948399394972 -0.2913057526161784) (-0.8755948399394972 -0.2913057526161784) (-0.8755948399394972 -1.4966988999433553) (-0.8755948399394972 0.00825926594520543) (-0.8755948399394972 -0.2940985014512921) (0.08518168064943167 -0.2940985014512921) (0.08518168064943167 -0.2940985014512921))))

(test-random "prog2-density"
  (run 1 (total-density q x)
    (prog2-density total-density (list x q))
    (prog2 x q))
  '((-2.528524373603396 -1.2269862731156183 -1.1740941342295155)))

(test-random "prog2-chain-1-c"
  (run 1 (ls)
    (fresh (x q)
      (== 2.0 x) ; user's responsibility to pass in the correct value for the condition...
      (== 1.2 q)
      (chain 10 (list x q) prog2-proposal-c prog2-density ls)))
  '(((2.0 1.2) (2.0 1.2) (2.0 1.2) (2.0 1.2) (2.0 1.2) (2.0 1.2) (2.0 1.9971409831526883) (2.0 2.584289087323319) (2.0 2.584289087323319) (2.0 2.8431895358811774))))

(test-random "prog2-density-c"
  (run 1 (total-density q x)
    (prog2-density total-density (list x q))
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
    (fresh (b q x y)
      (== #f b)
      (== (list 0.7) q)
      (chain 12 (list b q x y) prog4-proposal prog4-density ls)))
  '(((#f (0.7) 0.7 _.0)
     (#f (0.7) 0.7 _.0)
     (#f (0.33237604021746425) 0.33237604021746425 _.1)
     (#f (0.49624361083488844) 0.49624361083488844 _.2)
     (#f (0.8177294233977477) 0.8177294233977477 _.3)
     (#f (0.41107510704973493) 0.41107510704973493 _.4)
     (#f (0.8762533555943044) 0.8762533555943044 _.5)
     (#f (0.9344582655547984) 0.9344582655547984 _.6)
     (#f (0.9344582655547984) 0.9344582655547984 _.6)
     (#f (0.7661712866286206) 0.7661712866286206 _.7)
     (#f (0.944030636116415) 0.944030636116415 _.8)
     (#f (0.766216211761046) 0.766216211761046 _.9))))

(test-random "geom-density-1"
  (run 1 (total-density q x b*)
    (== 3 q)
    (== 0.5 x)
    (geom-density total-density (list x q b*)))
  '((-2.772588722239781 3 0.5 (#f #f #f #t))))

(test-random "geom-1"
  ;; Because we are not conditioning on q, this query can diverge.
  ;; This is because if geom-density associates q with 0, and
  ;; geom associates q with 1, geom-density will then associate q with 1, 2, ...
  ;; while geom is most likely to associate q with small values (mostly 0).
  ;;
  ;; The correct way to avoid this problem is to use chain, as in the
  ;; other examples above.  This will require implementing geom-proposal.
  (run 1 (total-density q x b*)
    (== 0.5 x)
    (geom-density total-density (list x q b*))
    (geom x q))
  '((-0.6931471805599453 0 0.5 (#t))))


(test "lift-prog1"
  (lift-variable
   '(define prog1
     (lambda (q)
       (fresh (x)
         (normal 0.0 1.0 x)
         (normal x 1.0 q)))))
  '(define prog1-var-lifted
     (lambda (x q)
       (fresh ()
         (normal 0.0 1.0 x)
         (normal x 1.0 q)))))

(test "lift-2"
  (lift-variable
   '(define prog
     (lambda ()
       (fresh (x)
         (normal 0.0 1.0 x)))))
  '(define prog-var-lifted
     (lambda (x)
       (fresh ()
         (normal 0.0 1.0 x)))))

(test "lift-3"
  (lift-variable
   '(define prog
      (lambda ()
        (fresh (x)
          (== #t x)))))
  '(define prog-var-lifted
     (lambda ()
       (fresh (x)
         (== #t x)))))

(test "lift-4"
  (lift-variable
   '(define prog
      (lambda ()
        (fresh (x)
          (normal 0.0 1.0 x)
          (normal 0.0 2.0 x)))))
  '(define prog-var-lifted
     (lambda (x)
       (fresh ()
          (normal 0.0 1.0 x)
          (normal 0.0 2.0 x)))))

(test "lift-5"
  (lift-variable
   '(define prog
      (lambda ()
        (fresh ()
          (fresh (x)
            (normal 0.0 1.0 x))
          (fresh (x)
            (normal 0.0 2.0 x))))))
  '(define prog-var-lifted
     (lambda (x1 x2)
       (fresh ()
         (normal 0.0 1.0 x1)
         (normal 0.0 2.0 x2)))))

(test "lift-6"
  (lift-variable
   '(define prog
      (lambda ()
        (fresh ()
          (fresh (x)
            (normal 0.0 1.0 x))
          (fresh (y)
            (normal 0.0 2.0 y))))))
  '(define prog-var-lifted
     (lambda (x y)
       (fresh ()
         (normal 0.0 1.0 x)
         (normal 0.0 2.0 y)))))

(test "lift-prog4"
  (lift-variable
   '(define prog4
      (lambda (b q)
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
               (== (list x) q)]))))))
  '(define prog4-var-lifted
     (lambda (q b y x)
       (fresh ()
         (flip 0.5 b)
         (conde
           ((== #t b)
            (normal 0.0 1.0 x)
            (normal 0.0 1.0 y)
            (== (list x y) q))
           ((== #f b)
            (uniform 0.0 1.0 x)
            (== (list x) q)))))))
