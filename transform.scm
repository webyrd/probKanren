(load "match.scm")
(load "utils.scm")
(load "test-check.scm")

;;; Transformation: quoted expression to quoted expression


;;; Single-site Proposal
(define make-ss-proposal
  (lambda (prog)
    (match prog
      [(define ,name
	 (lambda ,init-vars
	   ,body))
       (let ((new-name (concat-to-symbol-name name "-proposal"))
	     (new-vars (map (lambda (s)
			      (concat-to-symbol-name s "^"))
			    init-vars))
	     (num-vars (length init-vars)))
	 (let ((new-bodys (map (lambda (i)
                                 `[(== ,i choice)
                                   ,(make-ss-proposal-body
                                     body
                                     init-vars
                                     (list-ref init-vars i))])
			       (iota num-vars))))
           `(define ,new-name
                (lambda (init-vars new-vars)
                  (fresh ,(append init-vars new-vars)
                    (== (list ,@init-vars) init-vars)
                    (== (list ,@new-vars) new-vars)
                    (fresh (choice)     ; should be gensym'ed
                      (uniform 0 ,num-vars choice)
                      ,@(de-freshify
                         `(conde
                            ;; Concat better, still need the [(== 0 choice)]
                            ,@new-bodys))))))))])))

(define make-ss-proposal-body
  (lambda (body vars var-choice)
    (match body
      [(fresh ,x* . ,fresh-body*)
       (let ((fresh-body* (map
                           (lambda (fb)
                             (make-ss-proposal-body fb vars var-choice))
                           fresh-body*)))
         `(fresh ,x* . ,fresh-body*))]
      [(== ,x ,y) `(== ,(make-ss-proposal-body x vars var-choice)
		       ,(make-ss-proposal-body y vars var-choice))]
      [(normal ,mu ,sd ,var)
       (let ((new-var (concat-to-symbol-name var "^"))
	     (mu' (make-ss-proposal-body mu vars var-choice))
	     (sd' (make-ss-proposal-body sd vars var-choice)))
         (if (eq? var var-choice)
             `(normal ,mu' ,sd' ,new-var)
             `(fresh ()
                (== ,var ,new-var)
                (normal ,mu' ,sd' ,var))))])))



;;; Variable lifting


(define rename-proc
  ;; we are assuming gensyming has already been done!!!
  (lambda (name new-name body additional-args)
    (match body
      [,x (guard (symbol? x))
       (if (eq? x name)
           new-name
           x)]
      [(,proc . ,args) (guard (eq? proc name))
       (let ((additional-args (map (lambda (sym)
                                     (concat-to-symbol-name sym "$"))
                                   additional-args)))
         (let ((new-args (append (map (lambda (arg)
                                        (rename-proc name new-name arg additional-args))
                                      args)
                                 additional-args)))
           `(fresh ,additional-args
              (,new-name . ,new-args))))]
      [(,a . ,d)
       (cons (rename-proc name new-name a additional-args)
             (rename-proc name new-name d additional-args))]
      [,else else])))

(define lift-variable
  (lambda (prog)
    (match prog
      [(define ,name
	 (lambda ,arg*
	   ,body))
       (let ((new-name (concat-to-symbol-name name "-var-lifted")))
         (let ((vars-body (lift-variable-body body '())))
           (let ((vars     (cdr vars-body))
                 (new-body (car vars-body)))
             (let ((new-body (de-freshify new-body)))
               (let ((new-body
                      (match new-body
                        [((fresh ,args . ,e*))
                         `((fresh ,args . ,e*))]
                        [,else `((fresh () ,@new-body))])))
                 (let ((additional-args (diff vars arg*)))
                   (let ((new-arg* (append arg* additional-args)))
                     (let ((new-body (rename-proc name new-name new-body additional-args)))
                       `(define ,new-name
                          (lambda ,new-arg*
                            ,@new-body))))))))))])))

(define lift-variable-body
  (lambda (body vars)
    (match body
      [(== ,e1 ,e2)
       (cons `(== ,e1 ,e2) vars)]
      [(fresh ,args* . ,e*)
       (let ((vars-body (map (lambda (x) (lift-variable-body x vars)) e*)))
         (let ((new-vars (union* (map cdr vars-body)))
               (new-e*   (map car vars-body)))
           (cons `(fresh ,(diff args* new-vars) ,@new-e*) new-vars)))]
      [(conde . ,c*)
       (let ((vars-clauses (map (lambda (c) (lift-variable-body `(fresh () . ,c) vars)) c*)))
         (let ((new-vars (union* (map cdr vars-clauses)))
               (new-c* (map car vars-clauses)))
           (cons `(conde . ,(map list new-c*)) new-vars)))]
      [(normal ,_ ,__ ,x)
       (cons `(normal ,_ ,__ ,x) (union (list x) vars))]
      [(uniform ,_ ,__ ,x)
       (cons `(uniform ,_ ,__ ,x) (union (list x) vars))]
      [(flip ,_ ,x)
       (cons `(flip ,_ ,x) (union (list x) vars))]
      [(pluso ,_ ,__ ,___)
       (cons `(pluso ,_ ,__ ,___) vars)]
      [(,proc . ,args) ; user-defined or unknown procedure call (including recursive call)
       (letrec ((make-var-set (lambda (args)
                                (let loop ((args args)
                                           (vars '()))
                                  (cond
                                    [(null? args) vars]
                                    [(symbol? (car args))
                                     (if (memq (car args) vars)
                                         (loop (cdr args) vars)
                                         (loop (cdr args) (cons (car args) vars)))]
                                    [else (loop (cdr args) vars)])))))
         (cons `(,proc . ,args) (union (make-var-set args) vars)))])))

(define de-freshify
  (lambda (body)
    (match body
      [(fresh () . ,e*) (map-append de-freshify e*)]
      [(fresh ,args* . ,e*)
       (let ((new-e* (map-append de-freshify e*)))
         (list `(fresh ,args* . ,new-e*)))]
      [(conde . ,c*)
       (list `(conde . ,(map (lambda (c) (map-append de-freshify c)) c*)))]
      [,else (list else)])))



;;; Density transformation

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
      [(== ,e1 ,e2) `(== ,e1 ,e2)]
      [(fresh ,args* . ,e*)
       `(fresh ,args* ,@(map (lambda (e) (make-density-body e var-map)) e*))]
      [(conde . ,c*)
       `(conde ,@(map (lambda (c)
                        (map (lambda (g)
                               (make-density-body g var-map))
                             c))
                      c*))]
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


;; For prog3,

'(define prog3
   (lambda (b x)
     (fresh ()
       (flip 0.6 b)
       (conde
         [(== #t b) (normal 0.0 1.0 x)]
         [(== #f b) (uniform 0.0 1.0 x)]))))

;; becomes

'(define prog3-density
   (lambda (total-density vars)
     (fresh (b x)
       (== (list b x) vars)
       (fresh (db dx)
         (fresh ()
           (flip-density 0.6 b db)
           (conde
             [(== #t b) (normal-density 0.0 1.0 x dx)]
             [(== #f b) (uniform-density 0.0 1.0 x dx)]))
         (sumo (list db dx) total-density)))))


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
            (== (list x) q)])))))

'(define prog4-lifted
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

'(define prog4-density
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
                (== (list x) q)])))
         ;;
         (sumo (list db dx dy) total-density)))))
