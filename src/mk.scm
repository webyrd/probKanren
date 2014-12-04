;; Vicare doesn't seem to support random of a fixnum--need to use Chez for now.

(define-syntax rhs
  (syntax-rules ()
    ((_ x) (cdr x))))

(define-syntax lhs
  (syntax-rules ()
    ((_ x) (car x))))

(define-syntax size-s
  (syntax-rules ()
    ((_ x) (length x))))

(define-syntax var
  (syntax-rules ()
    ((_ x) (vector x))))

(define-syntax var?
  (syntax-rules ()
    ((_ x) (vector? x))))


(define empty-s '())
(define empty-sk/c-ls '())
(define empty-rp-ls '())
(define empty-c `(,empty-s ,empty-sk/c-ls  ,empty-rp-ls))

(define make-rp list)

(define get-sample
  (lambda (rp)
    (car rp)))

(define get-log-density
  (lambda (rp)
    (cadr rp)))

(define get-x
  (lambda (rp)
    (caddr rp)))

(define get-rest-args
  (lambda (rp)
    (cdddr rp)))

(define get-s
  (lambda (c)
    (car c)))

(define get-sk/c-ls
  (lambda (c)
    (cadr c)))

(define get-rp-ls
  (lambda (c)
    (caddr c)))

(define ext-sk/c-ls
  (lambda (sk c)
    (let ((s (get-s c))
          (sk/c-ls (get-sk/c-ls c))
          (rp-ls (get-rp-ls c)))
      `(,s ((,sk ,c) . ,sk/c-ls) ,rp-ls))))

(define ext-rp-ls
  (lambda (rp c)
    (let ((s (get-s c))
          (sk/c-ls (get-sk/c-ls c))
	  (rp-ls (get-rp-ls c)))
      `(,s ,sk/c-ls ,(cons rp rp-ls)))))

(define update-s
  (lambda (s c)
    (let ((sk/c-ls (get-sk/c-ls c))
	  (rp-ls (get-rp-ls c)))
      `(,s ,sk/c-ls ,rp-ls))))


(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (ext-s x v s)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v) 
         (or 
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        (else #f)))))


(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (assq u S)) =>
       (lambda (pr) (walk (rhs pr) S)))
      (else u))))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        (else v)))))


(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify 
                    (car u) (car v) s)))
           (and s (unify 
                    (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))


(define ==
  (lambda (u v)
    (lambda (sk fk c)
      (let ((s (get-s c)))
        (let ((s (unify u v s)))
          (if s
              (sk fk (update-s s c))
              (retry fk c)))))))

(define conj
  (lambda (g1 g2)
    (lambda (sk fk c)
      (g1 (lambda (fk^ c^) (g2 sk fk^ c^)) fk c))))

(define conj* (lambda args (conj*-aux args)))

(define conj*-aux
  (lambda (g*)
    (cond
      ((null? g*) succeed)
      ((null? (cdr g*)) (car g*))
      (else (conj (car g*) (conj*-aux (cdr g*)))))))


(define-syntax fresh
  (syntax-rules ()
    [(_ (x* ...) g g* ...)
     (let ((x* (var 'x*)) ...)
       (conj* g g* ...))]))

;; TODO
;;
;; might need to invoke a special fk if the constraints introduced
;; result in a low probability trace
;;
;; probably need to pass some or all of the sampled values to the
;; continuation when backtracking above a conde, so we can re-use as
;; much of the sample information as possible.
#|
(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g0* ...) (g* g** ...) ...)
     (lambda (sk fk c)
       (letrec ((sk^ (lambda (fk^ c^)
                       (let ((g-ls (list (conj* g0 g0* ...)
                                         (conj* g* g** ...)
                                         ...)))
                         (let ((g (list-ref g-ls (random (length g-ls)))))
                           (let ((c^ (ext-sk/c-ls sk^ c^)))
                             (g sk fk^ c^)))))))
         (sk^ fk c)))]))
|#

(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g0* ...) (g* g** ...) ...)
     (lambda (sk fk c)
       (letrec ((sk^ (lambda (fk^ c^)
                       (let ((g-ls (list (conj* g0 g0* ...)
                                         (conj* g* g** ...)
                                         ...)))
                         (letrec ((c^/g-th (lambda ()
                                             (let ((make-conde-sample
                                                    (lambda ()
                                                      (let ((c^/g (c^/g-th)))
                                                        (let ((c^ (car c^/g))
                                                              (g (cdr c^/g)))
                                                          (g sk fk^ c^)))))
                                                   (conde-density
                                                    (lambda (x-ignored)
                                                      (log (/ (length g-ls))))))
                                               (let ((rp (make-rp make-conde-sample conde-density 'x-ignore-from-conde)))
                                                 (let ((c^ (ext-sk/c-ls sk^ c^)))
                                                   (let ((c^ (ext-rp-ls rp c^)))
                                                     (let ((g (list-ref g-ls (random (length g-ls)))))
                                                       (cons c^ g)))))))))
                           (let ((c^/g (c^/g-th)))
                             (let ((c^ (car c^/g))
                                   (g (cdr c^/g)))
                               (g sk fk^ c^))))))))
         (sk^ fk c)))]))


(define uniform-sample
  (lambda (lo hi)
    (let ((samp (+ (random (- hi lo)) lo)))
      samp)))

(define uniform-log-density
  (lambda (x lo hi)
    (log (/ (- hi lo)))))

(define uniform
  (lambda (lo hi x)
    (lambda (sk fk c)
      (let ((rp (make-rp uniform-sample uniform-log-density x lo hi)))
        (sk fk (ext-rp-ls rp c))))))


(define flip-sample
  (lambda (p)
    (let ((samp (random 1.0)))
      (<= samp p))))

(define flip-log-density
  (lambda (x p)
    (log (if x p (- 1 p)))))

(define flip
  (lambda (p x)
    (lambda (sk fk c)
      (let ((rp (make-rp flip-sample flip-log-density x p)))
        (sk fk (ext-rp-ls rp c))))))

(define solve-rp-constraints
  ;; fake goal that runs last in run-mh  
  (lambda (sk fk c)
;    (printf "solve-rp-constraints c: ~s\n" c)
    (let loop ((rp-ls (get-rp-ls c))
               (s (get-s c)))
      (cond
        [(null? rp-ls) (sk fk (list (update-s s c)
                                    (get-samplable-rp-ls s c)))]
        [else
         (let ((rp (car rp-ls)))
           (let ((x (get-x rp))
                 (rest-args (get-rest-args rp)))
             (let ((x (walk* x s))
                   (rest-args (walk* rest-args s)))
               (cond
                 [(andmap ground? rest-args)
                  (if (ground? x)
                      (loop (cdr rp-ls) s)
                      (let ((sample (get-sample rp)))
                        (let ((samp (apply sample rest-args)))
                          (loop (cdr rp-ls) (ext-s x samp s)))))]
                 [(can-any-rp-can-be-processed? (cdr rp-ls) s)
                  (loop (append (cdr rp-ls) (list rp)) s)]
                 [else (error 'solve-rp-constraints "can't make progress with rps")]))))]))))

(define can-any-rp-can-be-processed?
  (lambda (rp-ls s)
    (let loop ((rp-ls rp-ls))
      (cond
        [(null? rp-ls) #f]
        [(let ((rp (car rp-ls)))
           (let ((rest-args (get-rest-args rp)))
             (let ((rest-args (walk* rest-args s)))
               (andmap ground? rest-args))))
         #t]
        [else (loop (cdr rp-ls))]))))

(define ground?
  (lambda (t)
    (cond
      [(var? t) #f]
      [(pair? t)
       (and (ground? (car t)) (ground? (cdr t)))]
      [else #t])))

(define find-rp-info
  (lambda (x rp-ls)
    (cond
      [(null? rp-ls) (error 'find-rp-info "could not find rp")]
      [(eq? (caddr (car rp-ls)) x)
       (car rp-ls)]
      [else (find-rp-info x (cdr rp-ls))])))

(define resample
  (lambda (rp-ls fk c)
    (let ((rp (list-ref rp-ls (random (length rp-ls)))))
      ;; (printf "rp 1: ~s\n" rp)
      (let ((resample-proc (car rp))
            (args (cdddr rp)))
        ;; (printf "args: ~s\n" args)
        (let ((density-proc (cadr rp))
              (x/args (cddr rp)))
          ;; (printf "x/args: ~s\n" x/args)
          (let ((x (car x/args)))
            ;; (printf "x: ~s\n" x)
            (let ((s (get-s c)))
              ;; (printf "s: ~s\n" s)
              (let ((R (apply density-proc (walk* x/args s))))
                ;; (printf "R: ~s\n" R)
                (let ((s-x (remove-from-s x s)))
                  (let ((val (apply resample-proc (walk* args s-x))))
                    ;; (printf "val: ~s\n" val)
                    (let ((s (cons (cons x val) s-x)))
                      (let ((F (apply density-proc (walk* x/args s))))
                        (list (update-s s c)
                              R
                              F)))))))))))))

(define remove-from-s
  (lambda (x s)
    (let ((pr (assq x s)))
      (if pr
          (remq pr s)
          s))))

(define get-samplable-rp-ls
  (lambda (s c)
    (let ((s-old (get-s c))
          (rp-ls (get-rp-ls c)))
      (let ((s-prefix (get-subst-prefix s-old s)))
        ;; (printf "s-prefix: ~s\n" s-prefix)
        (let loop ((rp-ls rp-ls)
                   (acc '()))
          (cond
            ((null? rp-ls)
             ;; (printf "acc: ~s\n" acc)
             acc)
            (else (let ((rp (car rp-ls)))
;                    (printf "rp: ~s\n" rp)
                    (let ((x (caddr rp)))
;                      (printf "x: ~s\n" x)
                      (cond
                        ((eq? 'x-ignore-from-conde x)
                         (loop (cdr rp-ls) (cons rp acc)))
                        ((assq x s-prefix)
                         (loop (cdr rp-ls) (cons rp acc)))
                        (else (loop (cdr rp-ls) acc))))))))))))

(define get-subst-prefix
  (lambda (s-old s-new)
    (cond
     [(eq? s-old s-new) '()]
     [else (cons (car s-new)
		 (get-subst-prefix s-old
				   (cdr s-new)))])))


(define-syntax run-mh
  (syntax-rules ()
    [(_ ne (x) g g* ...)
     (let ((n ne)
           (x (var 'x)))
       (let ((ans ((fresh () g g* ... solve-rp-constraints)
                   (lambda (fk c)
                     (list fk c))
                   (lambda () '())
                   empty-c)))
         (let loop ((n n)
                    (ans ans)
                    (ls '()))
           (cond
             ((zero? n) (reverse ls))
             ((null? ans) (reverse ls))
             (else
              (let ((fk (car ans))
                    (c/rp-ls (cadr ans)))
                ;; (printf "c/rp-ls: ~s\n" c/rp-ls)
                (let ((c (car c/rp-ls))
                      (rp-ls (cadr c/rp-ls)))
                  ;; (printf "cdr of c/rp-ls: ~s\n" (cdr c/rp-ls))
                  ;; (printf "rp-ls run-mh: ~s\n" rp-ls)
                  (loop
                   (sub1 n)
                   (if (null? rp-ls)
                       ans
                       (let ((c^/R/F (resample rp-ls fk c)))
                         (let ((c^ (car c^/R/F))
                               (R (cadr c^/R/F))
                               (F (caddr c^/R/F)))                                                    
                           (if (reject-sample? c^ c R F)
                               (list fk c/rp-ls) ;; do we really need this fk?
                               (list fk (list c^ rp-ls)) ;; do we really need this fk?
                               )
                           )))
                   (cons (reify x (get-s c)) ls)))))))))]))

(define reject-sample?
  (lambda (c^ c R F)
    (let ((rp-ls^ (get-rp-ls c^))
          (rp-ls (get-rp-ls c)))
      (let ((mproc
             ;; mproc takes a constraint store and an rp, and
             ;; calculates the log probability of that rp
             (lambda (c)
               (lambda (rp-info)
                 (let ((density-proc (cadr rp-info))
                       (x/args (cddr rp-info)))
                   (apply density-proc (walk* x/args (get-s c))))))))
        ;; The 'let' below corresponds to line 12 of Figure 2 from
        ;; page 3 of 'Lightweight Implementations of Probabilistic
        ;; Programming Languages Via Transformational'
        ;; (http://web.stanford.edu/~ngoodman/papers/lightweight-mcmc-aistats2011.pdf)
        ;;
        ;; D and D' are equivalent to rp and rp^          
        (let ((ll^ (apply + (map (mproc c^) rp-ls^)))
              (ll (apply + (map (mproc c) rp-ls)))                
              (rp-len^ (log (length rp-ls^)))
              (rp-len (log (length rp-ls)))
              (ll-stale (apply + (map (mproc c) (set-diff rp-ls rp-ls^))))
              (ll-fresh (apply + (map (mproc c) (set-diff rp-ls^ rp-ls)))))
          (let ((u (random 1.0)))
            (> (log u)
               ;; intuitively, new - old...
               (+ (- ll^ ll) (- R F) (- rp-len^ rp-len) (- ll-stale ll-fresh)))))))))

;; quadratic algorithm---boo!
(define set-diff
  ;; subtract the elements of s2 from s1
  (lambda (s1 s2)
    (cond
      ((null? s2) s1)
      (else (set-diff (remq (car s2) s1) (cdr s2))))))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (size-s s)) s))
        ((pair? v) (reify-s (cdr v)
                     (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v s)
    (let ((v (walk* v s)))
      (walk* v (reify-s v empty-s)))))

(define succeed
  (lambda (sk fk c)
    (sk fk c)))

(define fail
  (lambda (sk fk c)
    (fk)))


;; interesting test--what does this mean?
;;
;; (run 1 (q) (flip 0.00001 q) (flip 0.999999 q))
;;
;; vs.
;;
;; (run 1 (q) (flip 0.999999 q) (flip 0.00001 q))
;;
;; Are these equivalent?  Does this make sense?


#!eof

;; Standard miniKanren run/run* is now deprecated.  Need to implement
;; run using the approach in Ken and Oleg's 'Monolingual Probabilistic
;; Programming Using Generalized Coroutines'
;; (http://okmij.org/ftp/kakuritu/#uai2009)

(define-syntax run*
  (syntax-rules ()
    [(_ (x) g g* ...)
     (let ((x (var 'x)))
       ((fresh () g g* ...)
        (lambda (fk c)
          (cons (reify x (get-s c)) (fk)))
        (lambda () '())
        empty-c))]))

(define-syntax run
  (syntax-rules ()
    [(_ ne (x) g g* ...)
     (let ((n ne)
           (x (var 'x)))
       (let ((ans ((fresh () g g* ...)
                   (lambda (fk c)
                     (list fk c))
                   (lambda () '())
                   empty-c)))
         (let loop ((n n)
                    (ans ans)
                    (ls '()))
           (cond
             ((zero? n) (reverse ls))
             ((null? ans) (reverse ls))
             (else
              (let ((fk (car ans))
                    (c (cadr ans)))
                (let ((s (get-s c)))
                  (loop
                    (sub1 n)
                    (retry fk c)
                    (cons (reify x s) ls)))))))))]))

(define retry
  (lambda (fk c)
    (let ((sk/c-ls (get-sk/c-ls c)))
      (if (null? sk/c-ls)
          (fk)
          (let ((pick (random (length sk/c-ls))))
            (let ((sk/c (list-ref sk/c-ls pick)))
              (let ((sk (car sk/c))
                    (c (cadr sk/c)))
                (sk fk c))))))))
