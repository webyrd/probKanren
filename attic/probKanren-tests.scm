(load "mkdefs.scm")
(load "test-check.scm")

(test "run-mh-1"
  (run-mh (x)
    (== x 5))
  '(5))

(test "run-mh-2"
  (run-mh (x)
    (uniform 0 1 x))
  '((_.0
     (query-variable #(x))
     (subst ())
     (uniform 0 1 #(x) . #<procedure [char 5154 of mk.scm]>))))

(test "run-mh-3"
  (run-mh (x)
    (fresh (b)
      (flip x b)
      (== b #t))
    (uniform 0 1 x))
  '((_.0
     (query-variable #(x))
     (subst ((#(b) . #t)))
     (uniform 0 1 #(x) . #<procedure [char 5154 of mk.scm]>)
     (flip #(x) #t . #<procedure [char 4866 of mk.scm]>))))

#!eof

;; Stepping through run-mh-3:

(run-mh (p)          ; s: () f: () u: ()   also, q: #(p)
  (fresh (x)         ; s: () f: () u: ()
    (flip p x)       ; s: () f: ((flip #(p) #(x) . ,(lambda (p) (flip-sample p)))) u: ()
    ;; now solve constraints:  can't solve the flip constraint, since #(p) isn't ground
    (== x #t))       ; s: ((#(x) . #t)) f: ((flip #(p) #(x) . ,(lambda (p) (flip-sample p)))) u: ()
    ;; now solve constraints:
    ;; the flip constraint is now equivalent to (flip #(p) #t . ,(lambda (p) (flip-sample p)))
    ;; Still can't solve the flip constraint yet.
  (uniform 0 1 p))   ; s: ((#(x) . #t))
                     ; f: ((flip #(p) #(x) . ,(lambda (p) (flip-sample p))))
                     ; u: ((uniform 0 1 #(p) . ,(lambda (lo hi) (uniform-sample lo hi))))
    ;; now solve constraints
    ;; still can't solve flip constraint, since #(p) isn't ground yet.
    ;; But we can solve the uniform constraint
    ;; (uniform 0 1 #(p) . ,(lambda (lo hi) (uniform-sample lo hi)))
    ;; since 0 and 1 are ground terms.
    ;; Since #(p) is fresh, we aren't conditioning, and therefore need to sample.
    ;; Call the procedure produced by (lambda (lo hi) (uniform-sample lo hi)) with arguments 0 and 1.
    ;; Equivalent to calling (uniform-sample 0 1).
    ;; Let's say this call returns 0.3
    ;; Extend s with an association between #(p) and the result of the call, 0.3.
    ;; Note we don't need to unify #p with 0.3--we can just extend the s.
    ;; Here is the new constraint store:
    ;;   s: ((#(p) . 0.3) (#(x) . #t))
    ;;   f: ((flip #(p) #(x) . ,(lambda (p) (flip-sample p))))
    ;;   u: ((uniform 0 1 #(p) . ,(lambda (lo hi) (uniform-sample lo hi))))
    ;; Since the constraint store changed, we haven't reached a fix point; try solving again!
