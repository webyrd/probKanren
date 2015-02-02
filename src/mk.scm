;; Vicare doesn't seem to support random of a fixnum--need to use Chez for now.

;; TODO Reconsider the (if (= ll-stale ll-fresh) ...) logic in
;; 'reject-sample?'.  Is this necessary?  If so, how to make it work,
;; without breaking the Uniform-Mixture test in mktests.scm


;; The delayed-goal form (delayed-goal t g) creates a goal equivalent
;; to goal 'g', but whose execution is delayed until term 't' is fully
;; ground.  See example usage in mktests.scm.

;; (only partially implemented, but doesn't break anything)
;;
;; TODO
;;
;; * whenever the substitution is extended, including within
;; solve-rp-constraints, iterate through the delayed goals, and run
;; any goals that are now runnable:
;; ** after a successful == call
;; ** 
;;
;;
;; * if we finish solve-rp constraints, and the list of delayed goals
;; is non-empty, signal an error!  There is no way to calculate the
;; answer.
(define-syntax delayed-goal
  (syntax-rules ()
    ((_ te ge)
     (let ((t te)
           (g ge))
       (lambda (sk fk c)
         (let ((t (walk* t (get-s c))))
           (if (ground? t)
               (g sk fk c)
               (sk fk (ext-delayed-ls (cons t g) c)))))))))


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
(define empty-sk/c/conde-size-ls '())
(define empty-rp-ls '())
(define empty-delayed-ls '())
(define empty-c `(,empty-s ,empty-sk/c/conde-size-ls ,empty-rp-ls ,empty-delayed-ls))

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

(define get-sk/c/conde-size-ls
  (lambda (c)
    (cadr c)))

(define get-conde-size-ls
  (lambda (c)
    (let ((sk/c/conde-size-ls (get-sk/c/conde-size-ls c)))
      (if (null? sk/c/conde-size-ls)
          '()
          (map caddr sk/c/conde-size-ls)))))

(define get-rp-ls
  (lambda (c)
    (caddr c)))

(define get-delayed-ls
  (lambda (c)
    (cadddr c)))

;; save the sk/c/conde-size for each conde encountered
;;
;; 'conde-size' is the number of conde clauses, which we use to
;; calculate the log probabilities
(define ext-sk/c/conde-size-ls
  (lambda (sk c conde-size)
    (let ((s (get-s c))
          (sk/c/conde-size-ls (get-sk/c/conde-size-ls c))
          (rp-ls (get-rp-ls c))
          (delayed-ls (get-delayed-ls c)))
      `(,s ((,sk ,c ,conde-size) . ,sk/c/conde-size-ls) ,rp-ls ,delayed-ls))))

(define ext-rp-ls
  (lambda (rp c)
    (let ((s (get-s c))
          (sk/c/conde-size-ls (get-sk/c/conde-size-ls c))
	  (rp-ls (get-rp-ls c))
          (delayed-ls (get-delayed-ls c)))
      `(,s ,sk/c/conde-size-ls ,(cons rp rp-ls) ,delayed-ls))))

(define ext-delayed-ls
  (lambda (delayed-goal c)
    (let ((s (get-s c))
          (sk/c/conde-size-ls (get-sk/c/conde-size-ls c))
	  (rp-ls (get-rp-ls c))
          (delayed-ls (get-delayed-ls c)))
      `(,s ,sk/c/conde-size-ls ,rp-ls ,(cons delayed-goal delayed-ls)))))

(define replace-delayed-ls
  (lambda (delayed-ls c)
    (let ((s (get-s c))
          (sk/c/conde-size-ls (get-sk/c/conde-size-ls c))
	  (rp-ls (get-rp-ls c)))
      `(,s ,sk/c/conde-size-ls ,rp-ls ,delayed-ls))))

(define update-s
  (lambda (s c)
    (let ((sk/c/conde-size-ls (get-sk/c/conde-size-ls c))
	  (rp-ls (get-rp-ls c))
          (delayed-ls (get-delayed-ls c)))
      `(,s ,sk/c/conde-size-ls ,rp-ls ,delayed-ls))))


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

(define solve-delayed-goals
  (lambda (sk fk c)
    (let ((delayed-ls (get-delayed-ls c)))
      (let loop ((delayed-ls-unseen delayed-ls)
                 (delayed-ls-seen '()))
        (cond
          ((null? delayed-ls-unseen)
           (let ((c (replace-delayed-ls delayed-ls-seen c)))
             (sk fk c)))
          (else
           (let ((delayed-t/g (car delayed-ls-unseen)))
             (let ((t (car delayed-t/g))
                   (g (cdr delayed-t/g)))
               (let ((t (walk* t (get-s c))))
                 (if (ground? t)
                     (let ((c^ (replace-delayed-ls
                                 (append
                                   delayed-ls-seen
                                   (cdr delayed-ls-unseen))
                                 c))
                           (sk^ (lambda (fk^ c^)
                                  (solve-delayed-goals sk fk^ c^))))
                       (g sk^ fk c^))
                     (loop (cdr delayed-ls-unseen)
                           (cons delayed-t/g delayed-ls-seen))))))))))))

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

#;(define ==
  (lambda (u v)
    (lambda (sk fk c)
      (let ((s (get-s c)))
        (let ((s (unify u v s)))
          (if s
              (let ((c (update-s s c)))
                (sk fk c))              
              (fk)))))))

(define ==
  (lambda (u v)
    (lambda (sk fk c)
      (let ((s (get-s c)))
        (let ((s (unify u v s)))
          (if s
              (let ((c (update-s s c)))
                (solve-delayed-goals sk fk c))              
              (fk)))))))

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


;; TODO 12/4/2014
;;
;; ultimately want to handle conde and rp's uniformly if possible.
;; need to figure out how to make the types match for that to happen

(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g0* ...) (g* g** ...) ...)
     (lambda (sk fk c)
       (letrec ((sk^ (lambda (fk^ c^)
                       (let ((g-ls (list (conj* g0 g0* ...)
                                         (conj* g* g** ...)
                                         ...)))
                         (let ((conde-size (length g-ls)))
                           (let ((g (list-ref g-ls (random conde-size))))
                             (let ((c^ (ext-sk/c/conde-size-ls sk^ c^ conde-size)))
                               (g sk fk^ c^))))))))
         (sk^ fk c)))]))

(define uniform-sample
  (lambda (lo hi)
    (let ((samp (+ (random (- hi lo)) lo)))
      samp)))

(define uniform-log-density
  (lambda (x lo hi)
    (if (and (>= x lo) (<= x hi))
        (log (/ (- hi lo)))
        (log 0.0))))

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

(define sum-list
  (lambda (ls acc)
    (cond
     [(null? ls) '()]
     [else
      (cons
       (cons (caar ls) (+ acc (cdar ls)))
       (sum-list (cdr ls) (+ acc (cdar ls))))])))

(define drop-while
  (lambda (pred ls)
    (cond
     [(null? ls) '()]
     [(pred (car ls)) (drop-while pred (cdr ls))]
     [else (cons (car ls) (drop-while pred (cdr ls)))])))

(define categorical-sample
  (lambda (ls)
    (let ((total (apply + (map cdr ls)))
	  (sum-ls (sum-list ls 0)))
      (let ((u (uniform-sample 0.0 total)))
	(caar (drop-while (lambda (x) (> u (cdr x))) sum-ls))))))

(define categorical-log-density
  (lambda (x ls)
    (let ((total (apply + (map cdr ls)))
	  (p (assoc x ls)))
      (cond
       [p (log (/ (cdr p) total))]
       [else (log 0.0)]))))

(define categorical
  (lambda (ls x)
    (lambda (sk fk c)
      (let ((rp (make-rp categorical-sample categorical-log-density x ls)))
	(sk fk (ext-rp-ls rp c))))))

(define marsaglia
  (lambda ()
    (let ((x (uniform-sample -1.0 1.0))
	  (y (uniform-sample -1.0 1.0)))
      (let ((s (+ (* x x) (* y y))))
	(let ((q (sqrt (* -2.0 (log s) (/ s)))))
	  (cond
	   [(and (>= 1 s) (> s 0))
	    (cons (* x q) (* y q))]
	   [else (marsaglia)]))))))

(define normal-sample
  (lambda (mu sd)
    (cond
     [(< sd 0) (error 'normal-sample "given invalid parameters")]
     [else (+ mu (* sd (car (marsaglia))))])))

(define normal-log-density
  (lambda (x mu sd)
    (let ((sq (lambda (x) (* x x))))
      (let ((tau (/ (sq sd)))
	    (pi 3.141592653589793))
	(/ (+ (* (- tau) (sq (- x mu)))
	      (log (/ tau pi 2))) 2)))))

(define normal
  (lambda (mu sd x)
    (lambda (sk fk c)
      (let ((rp (make-rp normal-sample normal-log-density x mu sd)))
	(sk fk (ext-rp-ls rp c))))))

(define solve-rp-constraints
  ;; fake goal that runs last in run-mh
  (lambda (sk fk c)
    (let loop ((rp-ls (get-rp-ls c))
               (s (get-s c)))
      (cond
        [(null? rp-ls)
         (sk fk (list (update-s s c)
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
                          (let ((s (ext-s x samp s)))
                            (loop (cdr rp-ls) s)))))]
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
    (let ((sk/c/conde-size-ls (get-sk/c/conde-size-ls c)))
      (let ((total-len (+ (length rp-ls) (length sk/c/conde-size-ls))))
        (let ((ran (random total-len)))
          (if (>= ran (length rp-ls))
              (resample-conde fk c sk/c/conde-size-ls)
              (resample-rp rp-ls fk c)))))))


;; TODO figure out if these are the correct values of R and F
(define resample-conde
  (lambda (fk c sk/c/conde-size-ls)
    (let ((ran (random (length sk/c/conde-size-ls))))
      (let ((sk/c/conde-size (list-ref sk/c/conde-size-ls ran)))
        (let ((sk-ran (car sk/c/conde-size))
              (c-ran (cadr sk/c/conde-size)))
          ;; weird to return an 'ans' here!
          (let ((ans (sk-ran fk c-ran)))
            (if (null? ans)
                ;; TODO -- what to do for the null? case?
                (error 'resample-conde "what to do here?")
                (let ((fk (car ans))
                      (c/rp-ls^ (cadr ans)))
                  (let ((c^ (car c/rp-ls^))
                        (rp-ls^ (cadr c/rp-ls^)))
                    ;(printf "(get-conde-size-ls c^): ~s\n" (get-conde-size-ls c^))
                    ;(printf "(get-conde-size-ls c): ~s\n" (get-conde-size-ls c))
                    (list c^
                          (let ((R (apply +
                                          (map (lambda (n) (log (/ n)))
                                               (get-conde-size-ls c)))))
                            0) ;; R = reverse
                          ;; prob of transitioning from c^ to c
                          (let ((F (apply +
                                          (map (lambda (n) (log (/ n)))
                                               (get-conde-size-ls c^)))))
                            0) ;; F = forward
                          ;; prob of transitioning from c to c^
                          ))))))))))


#|
;; return the first n elements of ls
;; no longer used...
(define list-prefix
  (lambda (n ls)
    (cond
      [(zero? n) '()]
      [else (cons (car ls) (list-prefix (sub1 n) (cdr ls)))])))
|#

(define resample-rp
  (lambda (rp-ls fk c)
    (let ((orig-c c))
      (let ((rp (list-ref rp-ls (random (length rp-ls)))))
        (let ((resample-proc (car rp))
              (args (cdddr rp)))
          (let ((density-proc (cadr rp))
                (x/args (cddr rp)))
            (let ((x (car x/args)))
              (let ((s (get-s c)))
                (let ((R (apply density-proc (walk* x/args s))))
                  (let ((s-x (remove-from-s x s)))                    
                    (let ((val (apply resample-proc (walk* args s-x))))
                      (let ((s (ext-s x val s-x)))
                        (let ((c (update-s s c)))

                          ;; DELAYED GOALS STUFF
                  
                          ;; Once we remove x from s, any delayed goals that
                          ;; were dependent upon x (really, the rp being
                          ;; resampled) need to be re-run.  This means we have
                          ;; to keep the delayed goals around, even after they
                          ;; are run---just like we need to keep rp's around,
                          ;; even after the rp's have been run.

                          ;; We can tell which delayed goals need to be
                          ;; re-run, since their 't's will no longer be ground
                          ;; in s-x.  Observation: assuming delayed goals are
                          ;; deterministic (a reasonable assumption!), delayed
                          ;; goals are idempotent.  Re-running a previously
                          ;; run delayed goal is essentially an expensive
                          ;; no-op, since the resulting
                          ;; unifications/substitution extension will be the
                          ;; same.

;                          (let ((c (solve-delayed-goals c)))
;                            )
                          
                          (if c
                              (let ((F (apply density-proc (walk* x/args s))))
                                (list c
                                      R
                                      F))
                              ;; if a delayed goal failed, resample
                              (resample-rp rp-ls fk orig-c))
                          )))))))))))))

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
        (let loop ((rp-ls rp-ls)
                   (acc '()))
          (cond
            ((null? rp-ls) acc)
            (else (let ((rp (car rp-ls)))
                    (let ((x (caddr rp)))
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
                (let ((c (car c/rp-ls))
                      (rp-ls (cadr c/rp-ls)))
                  (loop
                   (sub1 n)
                   (if (and (null? rp-ls) (null? (get-sk/c/conde-size-ls c)))
                       ans
                       (let ((c^/R/F (resample rp-ls fk c)))
                         (let ((c^ (car c^/R/F))
                               (R (cadr c^/R/F))
                               (F (caddr c^/R/F)))
                           (if (reject-sample? c^ c R F)
                               (begin
                                 ;(printf "reject-sample => #t\n")
                                 (list fk c/rp-ls)) ;; do we really need this fk?
                               (begin
                                 ;(printf "reject-sample => #f\n")
                                 (list fk (list c^ rp-ls))) ;; do we really need this fk?
                               ))))
                   (cons (reify x (get-s c)) ls)))))))))]))

(define reject-sample?
  (lambda (c^ c R F)
    (let-values ([(ll-rp
                   ll-rp^
                   rp-len
                   rp-len^
                   ll-rp-stale
                   ll-rp-fresh)
                  (calculate-rp-ls-likelihoods c c^)]
                 [(log-conde-size-ls
                   log-conde-size-ls^
                   conde-size-ls-len
                   conde-size-ls-len^
                   ll-conde-stale
                   ll-conde-fresh)
                  (calculate-conde-size-ls-likelihoods c c^)])
      (let ((ll (+ ll-rp log-conde-size-ls))
            (ll^ (+ ll-rp^ log-conde-size-ls^))
            (rp-len (log (+ rp-len conde-size-ls-len)))
            (rp-len^ (log (+ rp-len^ conde-size-ls-len^)))
            (ll-stale (+ ll-rp-stale ll-conde-stale))
            (ll-fresh (+ ll-rp-fresh ll-conde-fresh)))
        ;(printf "ll^: ~s\n" ll^)
        ;(printf "ll: ~s\n" ll)
        ;(printf "R: ~s\n" R)
        ;(printf "F: ~s\n" F)
        ;(printf "rp-len^: ~s\n" rp-len^)
        ;(printf "rp-len: ~s\n" rp-len)
        (let ((u (random 1.0)))
          ;(printf "ll-stale: ~s\nll-fresh: ~s\n" ll-stale ll-fresh)
          (let ((log-u (log u))
                (sum (+ (- ll^ ll)
                        (- R F)
                        (- rp-len rp-len^)

                        ;; We arguably should include this term as
                        ;; well, but this causes problems with the
                        ;; Uniform-Mixture test in mktests.scm
                        ;; (instead of transitioning almost
                        ;; immediately from #t to #f, the test can
                        ;; produce 10000 #t's).  This may be due to
                        ;; ll-stale or ll-fresh being negative or
                        ;; positive infinity.
                        #;(if (= ll-stale ll-fresh)
			    0
			    (- ll-stale ll-fresh))

                        )))
            ;(printf "log-u: ~s\n" log-u)
            ;(printf "sum: ~s\n" sum)
            (or (nan? sum)
		(> log-u sum))))))))

(define calculate-rp-ls-likelihoods
  (lambda (c c^)
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
        (let ((ll (apply + (map (mproc c) rp-ls)))
              (ll^ (apply + (map (mproc c^) rp-ls^)))
              (rp-len (length rp-ls))
              (rp-len^ (length rp-ls^))
              (ll-stale (apply + (map (mproc c) (set-diff rp-ls rp-ls^))))
              (ll-fresh (apply + (map (mproc c) (set-diff rp-ls^ rp-ls)))))
          (values
           ll
           ll^
           rp-len
           rp-len^
           ll-stale
           ll-fresh))))))

(define calculate-conde-size-ls-likelihoods
  (lambda (c c^)
    (let ((conde-size-ls (get-conde-size-ls c))
          (conde-size-ls^ (get-conde-size-ls c^)))
      (let ((calc-conde-size-log (lambda (conde-size-ls)
                                   (apply +
                                          (map
                                           (lambda (conde-size) (log (/ conde-size)))
                                           conde-size-ls)))))
        (let ((log-conde-size-ls^ (calc-conde-size-log conde-size-ls^))
              (log-conde-size-ls (calc-conde-size-log conde-size-ls)))
          (let ((stale-conde-size-ls/fresh-conde-size-ls
                 (partition-stale/fresh-conde-size-ls-by-sk c c^)))
            (let ((stale-conde-size-ls (car stale-conde-size-ls/fresh-conde-size-ls))
                  (fresh-conde-size-ls (cadr stale-conde-size-ls/fresh-conde-size-ls)))
              (let ((ll-conde-stale (calc-conde-size-log stale-conde-size-ls))
                    (ll-conde-fresh (calc-conde-size-log fresh-conde-size-ls)))
                (let ((conde-size-ls-len (length conde-size-ls))
                      (conde-size-ls-len^ (length conde-size-ls^)))
                  (values
                    log-conde-size-ls
                    log-conde-size-ls^
                    conde-size-ls-len
                    conde-size-ls-len^
                    ll-conde-stale
                    ll-conde-fresh))))))))))


;; returns stale-conde-size-ls/fresh-conde-size-ls
(define partition-stale/fresh-conde-size-ls-by-sk
  (lambda (c cˆ)
    (let ((sk/c/conde-size-ls (get-sk/c/conde-size-ls c))
          (sk/c/conde-size-lsˆ (get-sk/c/conde-size-ls cˆ)))
      (let ((set-diff-proc (lambda (elem2)
                             (lambda (elem1)
                               (eq? (car elem1) (car elem2))))))
        (let ((stale (set-diff-by-proc
                      sk/c/conde-size-ls
                      sk/c/conde-size-lsˆ
                      set-diff-proc))
              (fresh (set-diff-by-proc
                      sk/c/conde-size-lsˆ
                      sk/c/conde-size-ls
                      set-diff-proc)))
          (let ((conde-size-ls-stale (map caddr stale))
                (conde-size-ls-fresh (map caddr fresh)))
            (list conde-size-ls-stale conde-size-ls-fresh)))))))

;; quadratic algorithm---boo!
(define set-diff
  ;; subtract the elements of s2 from s1
  (lambda (s1 s2)
    (cond
      ((null? s2) s1)
      (else (set-diff (remq (car s2) s1) (cdr s2))))))

(define set-diff-by-proc
  (lambda (s1 s2 p)
    (cond
      ((null? s2) s1)
      (else
       (set-diff-by-proc
         (remp (p (car s2)) s1)
         (cdr s2)
         p)))))

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
      (let ((v (walk* v (reify-s v empty-s))))
        ;(printf "v: ~s\n\n" v)
        v))))

(define succeed
  (lambda (sk fk c)
    (sk fk c)))

(define fail
  (lambda (sk fk c)
    (fk)))

(define-syntax project
  (syntax-rules ()
    [(_ (x* ...) g)
     (lambda (sk fk c)
       (let ((s (get-s c)))
         (let* ([x* (walk* x* s)] ...)
           (g sk fk c))))]))
