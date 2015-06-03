;;; This file was generated by writeminikanren.pl
;;; Generated at 2007-10-25 15:24:42

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (p) e) (lambda (p) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))


;; If we finish running all the goals, and the list of delayed goals
;; is non-empty, signal an error!  There is no way to calculate the
;; answer.

(define-syntax delayed-goal
  (syntax-rules ()
    ((_ te ge)
     (let ((t te)
           (g ge))
       (lambda (p)
         (let ((t (walk* t (p->s p))))
           (if (ground? t)
               (g p)
               (add-dg p (cons t g)))))))))

(define ground?
  (lambda (t)
    (cond
      ((var? t) #f)
      ((pair? t)
       (and (ground? (car t)) (ground? (cdr t))))
      (else #t))))

;; do we need to keep delayed goals around, after they have been run?
(define solve-delayed-goals
  (lambda (p)
    (let ((dg* (p->dg* p)))
      (let loop ((unseen-dg* dg*)
                 (seen-dg* '()))
        (cond
          ((null? unseen-dg*)
           (let ((p (update-p-with-dg* p seen-dg*)))
             p))
          (else
           (let ((delayed-t/g (car unseen-dg*)))
             (let ((t (car delayed-t/g))
                   (g (cdr delayed-t/g)))
               (let ((t (walk* t (p->s p))))
                 (if (ground? t)
                     (let ((p (update-p-with-dg*
                               p
                               (append
                                seen-dg*
                                (cdr unseen-dg*)))))
                       ;; I'm not sure about this part...
                       (solve-delayed-goals p))
                     (loop (cdr unseen-dg*)
                           (cons delayed-t/g seen-dg*))))))))))))




(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

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

(define empty-dg* '())

(define empty-p (list empty-s empty-dg*))

(define p->s (lambda (p) (car p)))
(define p->dg* (lambda (p) (cadr p)))

(define update-p-with-s
  (lambda (p s)
    (list s (p->dg p))))

(define update-p-with-dg*
  (lambda (p dg*)
    (list (p->s p) dg*)))

(define add-dg
  (lambda (p dg)
    (let ((dg* (p->dg* p)))
      (let ((dg* (cons dg dg*)))
        (update-p-with-dg* p dg*)))))


(define walk
  (lambda (u s)
    (cond
      ((and (var? u) (assq u s)) =>
       (lambda (pr) (walk (rhs pr) s)))
      (else u))))

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

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
  (lambda (v p)
    (let ((s (p->s p)))
      (let ((v (walk* v s)))
        (walk* v (reify-s v empty-s))))))

(define-syntax mzero 
  (syntax-rules () ((_) #f)))

(define-syntax inc 
  (syntax-rules () ((_ e) (lambdaf@ () e))))

(define-syntax unit 
  (syntax-rules () ((_ a) a)))

(define-syntax choice 
  (syntax-rules () ((_ a f) (cons a f))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((a^) e2) ((a f) e3))
     (let ((a-inf e))
       (cond
         ((not a-inf) e0)
         ((procedure? a-inf)  (let ((f^ a-inf)) e1))
         ((not (and (pair? a-inf)
                    (procedure? (cdr a-inf))))
          (let ((a^ a-inf)) e2))
         (else (let ((a (car a-inf)) (f (cdr a-inf))) 
                 e3)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ... 
            (lambdag@ (p)
              (unless (null? (p->dg p))
                (error 'run "Delayed goals list is non-empty at end of run"))              
              (cons (reify x p) '())))
          empty-p))))))

(define take
  (lambda (n f)
    (if (and n (zero? n)) 
      '()
      (case-inf (f)
        (() '())
        ((f) (take n f))
        ((a) a)
        ((a f)
         (cons (car a)
           (take (and n (- n 1)) f)))))))

(define ==
  (lambda (u v)
    (lambdag@ (p)
      (let ((s (p->s p)))
        (let ((s (unify u v s)))
          (if s
              (let ((p (update-p-with-s p s)))
                (solve-delayed-goals p))
              (mzero)))))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (p)
       (inc
         (let ((x (var 'x)) ...)
           (bind* (g0 p) g ...)))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((a) (g a))
      ((a f) (mplus (g a) (lambdaf@ () (bind (f) g)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (p) 
       (inc 
         (mplus* 
           (bind* (g0 p) g ...)
           (bind* (g1 p) g^ ...) ...))))))
 
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0 
                    (lambdaf@ () (mplus* e ...))))))
 
(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((a) (choice a f))
      ((a f^) (choice a (lambdaf@ () (mplus (f) f^)))))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (p)
       (inc
         (ifa ((g0 p) g ...)
              ((g1 p) g^ ...) ...))))))
 
(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (p)
       (inc
         (ifu ((g0 p) g ...)
              ((g1 p) g^ ...) ...))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* (unit a) g ...)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (p)
       (let ((s (p->s p)))
         (let ((x (walk* x s)) ...)
           ((fresh () g g* ...) p)))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
      (g succeed)
      ((== #f #f) fail))))
