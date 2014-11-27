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
;; need to repackage the sk/c list in terms of rp's
;;
;; might also need to be able to invoke a special fk if the
;; constraints introduced result in a low probability trace
;;
;; probably need to pass some or all of the sampled values to the
;; continuation when backtracking above a conde, so we can re-use as
;; much of the sample information as possible.
(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g0* ...) (g* g** ...) ...)
     (lambda (sk fk c)
       (letrec ((sk^ (lambda (fk^ c^)
                       (let ((g-ls (list (conj* g0 g0* ...)
                                         (conj* g* g** ...)
                                         ...)))
                         (let ((pick (random (length g-ls))))
                           (let ((g (list-ref g-ls pick)))
                             (let ((c^ (ext-sk/c-ls sk^ c^)))
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

;; TODO
;;
;; keep track of the extension to the original substitution caused by
;; sampling
(define solve-rp-constraints
  ;; fake goal that runs last in run-mh  
  (lambda (sk fk c)
    (printf "solve-rp-constraints c: ~s\n" c)
    (let loop ((rp-ls (get-rp-ls c))
               (s (get-s c)))
      (cond
        [(null? rp-ls) (sk fk (list (update-s s c)
				    (get-s c)))]
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

(define walk-rp
  (lambda (u S)
    (cond
     [(null? S) u]
     [(eq? (caddr (car S)) u)
      (car S)]
     [else (walk-rp u (cdr S))])))

(define resample
  (lambda (s-prefix fk c/old-s)
    (let ((c (car c/old-s))
	  (old-s (cadr c/old-s)))
      (cond
       [(null? s-prefix) c/old-s]
       [else
	(let ((index-s-prefix (random (length s-prefix))))
	  (let ((pr (list-ref s-prefix index-s-prefix)))
	    (let ((new-s-prefix (remq pr s-prefix)))
	      (let ((rp-ls (get-rp-ls c)))
		(let ((rp (walk pr rp-ls)))
		      0)))))]))))

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

(define get-subst-prefix
  (lambda (s-old s-new)
    (cond
     [(eq? s-old s-new) '()]
     [else (cons (car s-new)
		 (get-subst-prefix s-old
				   (cdr s-new)))])))
;; TODO
;;
;; needs to do resampling: keeping track of the extension to the
;; original substitution produced by sampling in solve-rp-constraints
;;
;; calculate log densities of rps to figure out whether to keep the
;; old substitution or the new substitution to answer the query
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
                    (c/old-s (cadr ans)))
		(let ((c (car c/old-s))
		      (old-s (cadr c/old-s)))
		  (let ((s (get-s c)))
		    (let ((s-prefix (get-subst-prefix old-s s)))
		      (loop
		       (sub1 n)
		       (resample s-prefix fk c/old-s)
		       (cons (reify x s) ls)))))))))))]))

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
