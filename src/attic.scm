;;; old & obsolete code


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
