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
(define empty-k/c-ls '())
(define empty-c `(,empty-s ,empty-k/c-ls))

(define get-s
  (lambda (c)
    (car c)))

(define get-k/c-ls
  (lambda (c)
    (cadr c)))

(define update-s
  (lambda (s c)
    (let ((k/c-ls (get-k/c-ls c)))
      `(,s ,k/c-ls))))


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
              (let ((c (update-s s c)))
                (sk fk c))
              (fk)))))))


#|
(define disj
  (lambda (g1 g2)
    (lambda (sk fk c)
      (g1 sk (lambda () (g2 sk fk c)) c))))
|#

(define conj
  (lambda (g1 g2)
    (lambda (sk fk c)
      (g1 (lambda (fk^ c^) (g2 sk fk^ c^)) fk c))))


#|
(define disj* (lambda args (disj*-aux args)))
|#

(define conj* (lambda args (conj*-aux args)))

#|
(define disj*-aux
  (lambda (g*)
    (cond
      ((null? g*) fail)
      ((null? (cdr g*)) (car g*))
      (else (disj (car g*) (disj*-aux (cdr g*)))))))
|#

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


#|
(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g0* ...) (g* g** ...) ...)
     (disj* (conj* g0 g0* ...) (conj* g* g** ...) ...)]))
|#

(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g0* ...) (g* g** ...) ...)
     (lambda (sk fk c)
       (let ((g-ls (list (conj* g0 g0* ...)
                         (conj* g* g** ...)
                         ...)))
         (let ((pick (random (length g-ls))))
           (let ((g (list-ref g-ls pick)))
             (g sk fk c)))))]))


(define-syntax run*
  (syntax-rules ()
    [(_ (x) g g* ...)
     (let ((x (var 'x)))
       ((fresh () g g* ...)
        (lambda (fk c)
          (cons (reify x (get-s c)) (fk)))
        (lambda () '())
        empty-c))]))


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
