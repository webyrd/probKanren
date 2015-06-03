;;; old & obsolete code


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

#|
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
                                               (let ((rp (make-rp make-conde-sample
                                                                  conde-density
                                                                  'x-ignore-from-conde)))
                                                 (let ((c^ (ext-sk/c-ls sk^ c^)))
                                                   (let ((c^ (ext-rp-ls rp c^)))
                                                     (let ((index (random (length g-ls))))
                                                       (let ((g (list-ref g-ls index)))
                                                         (cons c^ g))))))))))
                           (let ((c^/g (c^/g-th)))
                             (let ((c^ (car c^/g))
                                   (g (cdr c^/g)))
                               (g sk fk^ c^))))))))
         (sk^ fk c)))]))
|#



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
    (let ((sk/c/len-ls (get-sk/c/len-ls c)))
      (if (null? sk/c/len-ls)
          (fk)
          (let ((pick (random (length sk/c/len-ls))))
            (let ((sk/c/len (list-ref sk/c/len-ls pick)))
              (let ((sk (car sk/c/len))
                    (c (cadr sk/c/len))
                    (len (caddr sk/c/len)))
                (sk fk c))))))))
