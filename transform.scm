(load "match.scm")
(load "utils.scm")
(load "test-check.scm")

;;; Transformation: quoted expression to quoted expression

;;; Density transformation

(define lookup
  (lambda (x ls)
    (cdr (assq x ls))))

(define make-density-function
  ;; Takes a quoted expression representing a probKanren program.
  ;;
  ;; Returns a quoted probKanren program that acts as a density
  ;; function.
  (lambda (prog)
    (match prog
      [(define ,name
          (lambda ,arg*
            ,body))
       (let ((new-name
              (string->symbol
               (string-append
                (symbol->string name)
                "-density")))
             (total-density 'total-density)
             (vars 'vars)
             (var-map
              (map (lambda (x)
                     (cons x
                       (string->symbol
                        (string-append "d" (symbol->string x)))))
                   arg*)))
         `(define ,new-name
            (lambda (,total-density ,vars)
              (fresh ,arg*
                (== (list ,@arg*) ,vars)
                (fresh ,(map cdr var-map)
                  ,(make-density-body body var-map)
                  (sumo (list ,@(map cdr var-map)) ,total-density))))))])))

(define make-density-body
  (lambda (body var-map)
    (match body
      [(fresh ,args* ,[e*] ...)
       `(fresh ,args* ,e* ...)]
      [(flip ,_ ,x)
       `(flip-density ,_ ,x ,(lookup x var-map))]
      [(normal ,_ ,__ ,x)
       `(normal-density ,_ ,__ ,x ,(lookup x var-map))]
      [(uniform ,_ ,__ ,x)
       `(uniform-density ,_ ,__ ,x ,(lookup x var-map))])))

;;; We want to turn this:

'(define prog2
   (lambda (x q)
     (fresh ()
       (normal 0.0 1.0 x)
       (normal x 1.0 q))))

;;; into this:

'(define prog2-density
   (lambda (total-density vars)
     (fresh (x q)
       (== (list x q) vars)
       (fresh (dx dq)
         (fresh ()
           (normal-density 0.0 1.0 x dx)
           (normal-density x 1.0 q dq))
         (sumo (list dx dq) total-density)))))
