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
(define empty-rp '())
(define empty-c `(,empty-s ,empty-sk/c-ls  ,empty-rp))

(define get-s
  (lambda (c)
    (car c)))

(define get-rp
  (lambda (c)
    (caadr c)))

(define get-sk/c-ls
  (lambda (c)
    (cadr c)))

(define ext-sk/c-ls
  (lambda (sk c)
    (let ((s (get-s c))
          (sk/c-ls (get-sk/c-ls c)))
      `(,s ((,sk ,c) . ,sk/c-ls)))))

(define ext-rp/c-ls
  (lambda (rp c)
    (let ((s (get-s c))
	  (rp/c-ls (get-rp c)))
      `(,s ,sk (cons rp rp/c-ls)))))

(define update-s
  (lambda (s c)
    (let ((sk/c-ls (get-sk/c-ls c))
	  (rp/c-ls (get-rp c)))
      `(,s ,sk/c-ls ,rp/c-ls))))


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
  (lambda (lo hi) "stub"))

(define uniform-log-density
  (lambda (lo hi x) 0))

(define uniform
  (lambda (lo hi x)
    (lambda (sk fk c)
      (let ((s (get-s c)))
	0))))

(define flip-sample
  (lambda (p) "stub"))

(define flip-log-density
  (lambda (p x) p))

(define flip
  (lambda (p x)
    (lambda (sk fk c)
      #t)))

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
