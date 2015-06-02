



WELCOME TO RISEUP'S ETHERPAD!

 WARNING: this pad is accessible by anyone who has the address to this pad, if you used an obvious name for the pad, it could be guessed.
 WARNING: This pad will be DELETED if 30 days go by with no edits. There is NO WAY to recover the pad after this happens, so be careful!

  Riseup is a collective providing secure online communication tools for people and groups working on liberatory social change. If you appreciate the tools Riseup provides, please contribute! We rely on contributions by users to exist: http://riseup.net/donate


(run 10 (q) (fresh (uniform 0 1) (x) (flip x #t)))

(run 10 (q) (fresh (uniform 0 1) (x) (flip x #t) (== q x)))

(run 10 (uniform 0 1) (x) (flip x #t))

(run-with-distribution10 (uniform 0 1) (x) (flip x #t))

(mh-query (define x (uniform 0 1))
          (define q (flip x))
          q
          (eq x #t))
          
(mh-query (define x (uniform 0 1))
          (define q (flip x))
          x
          (eq q #t))

(run-mh (uniform 0 1) (x)
  (fresh (q)
    (flip x q)
    (== q #t)))

(run-mh (uniform 0 1) (x)
  (fresh (flip x) (q)
    (== q #t)))

(run-mh (x)
  (fresh (q)
    (flip x q)
    (== q #t))
  (uniform 0 1 x))

(run-mh (q)
    (uniform 0 1 q)
    (normal 0 1 q)
    (== q -1))
    
;; Mixture model

(run-mh 1000 (q)
    (conde
       ((uniform 0 1 q))
       ((uniform-discrete 0 1 q)))
    (== q 0.5))
    
(run-mh (q)
  (fresh (x)
    (conde
       ((uniform 0 1 x)
        (== q #t))
       ((uniform-discrete 0 1 x)
        (== q #f)))
    (== x 0.5))))
    

;; Traditional style

(define-syntax ifo
  (syntax-rules ()
    [(_ t c a)
     (conde
       [(== #t t) c]
       [(== #f t) a])]))

(run-mh (q)
  (fresh (x)
    (if q (uniform 0 1 x)
          (uniform-discrete 0 1 x))
    (== x 0.5))
  (flip 0.5 q))

;; Geometric model

(define geometric
  (lambda (p q)
     (fresh (x q1)
        (if x
            (== q 1)
            (== q (+ 1 (geometric p q1))))))
     

(define geometric
  (lambda (p q)
     (fresh (x)
        (flip p x)
        (conde
          [(== #t x) (== 1 q)]
          [(== #f x)
           (fresh (res)
             (+o 1 res q)
             (geometric p res)))))


(run-mh (q)
   (flip 0.2 q)
   (== q #t))

(run-mh (q)
   (geometric 0.7 q))

(run-mh (q)
   (uniform 0 1 q)
   (geometric q 5))
   
   
(flip)

(run 1 (q)
  (fresh (x)
    (uniform 0 1 x)
    (flip x q)))



;; turn this

(run-mh (x)
  (fresh (b)
    (flip x b)
    (== b #t))
  (uniform 0 1 x))

;; into this or the equivalent

(mh-query (define x (uniform 0 1)) ; program body
          (define b (flip x))      ; more program body
 
          x                        ; query variable
          (equal? b #t))           ; conditioning

(define r (flip 0.2 u))

(how-sampled? u) => scheme-function
((prob? u) #t) => 0.2

(()     ()) ; empty constraint store
 subst  XRPs
  ==    uniform,
        gaussian,
        flip,
        etc.
  
another representation
(()     ()         ()           ()   ...) ; empty constraint store
 subst  uniform    gausssian   flip   other XRPs
  ==

simplified representation
(()     ()         ()) ; empty constraint store
 subst  uniform    flip
  ==


;;;; Here's what Will is supposed to implement:

(run-mh (x)        ; (() () ()) ; empty
  (fresh (b)
    (flip x b)     ; (() () ((flip x b . (lambda (x) (flip-sample x)))
    (== b #t))     ; (((b . #t)) () ((flip x b . (lambda (x) (flip-sample x)))
  (uniform 0 1 x)) ; (((b . #t)) ((uniform 0 1 x . (lambda (low high) (uniform-sample low high))) ((flip x b . (lambda (x) (flip-sample x)))

;; final constraint store
 (((b . #t))
  ((uniform 0 1 x . [(lambda (low high) (uniform-sample   low high))
                     (lambda (low high) (uniform-log-prob low high)))
  ((flip x b . (lambda (x) (flip-sample x)))

from this final constraint store, do the following at reification time:

1. walk* the XRP constraints
2. find an XRP with ground values in all the 

;; need to figure out the constraint solving/constraint satisfaction rules.  For example, (uniform 0 1 10) should fail.

Example, starting with this constraint store from above.

 (((#<b> . #t))
  ((uniform 0 1 #<x> . (lambda (low high) (uniform-sample low high)))
  ((flip #<x> #<b> . (lambda (x) (flip-sample x)))

Step 1. walk* the XRP constraints in terms of the current substitution.

This gives us updated XRP constraints

((uniform 0 1 #<x> . (lambda (low high) (uniform-sample low high)))
((flip #<x> #t . (lambda (x) (flip-sample x))

Step 2. Pick an XRP with a logic variable in the "output position" and ground values as the other arguments.
In this case, (uniform 0 1 #<x> . (lambda (low high) (uniform-sample low high))

Call (uniform-sample 0 1), associate #<x> with the result (say 0.3) through unification.
(Could implement as (== #<x> (uniform-sample 0 1))

Substitution is now ((#<x> . 0.30) (#<b> . #t))

New constraint store looks like:

 (((#<x> . 0.30) (#<b> . #t))
  ()
  ((flip #<x> #t . (lambda (x) (flip-sample x)))

Do Step 1. again (walk*) on the remaining constraints.

  ()
  ((flip 0.30 #t . (lambda (x) (flip-sample x))

