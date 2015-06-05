(load "mk.scm")

;;; Hack: we will keep a list of variables bound through calls to
;;; random primitives (flip, uniform, normal, etc), which will be used
;;; during resampling.  we do this by stashing an association list in
;;; the substitution, bound to the 'fake' logic variable *rp-ls-var*.
;;; This association list contains associations between logic
;;; variables and the rp calls.  For example, (flip 0.6 x) might
;;; associate x with #f in the substitution, and also add an
;;; association between x and (flip 0.6 #f) in the *rp-ls-var* alist.
;;;
;;; Assuming the substitution is an association list, the resulting
;;; subst might look like:
;;;
;;; ((*rp-ls-var* . ((x . (flip 0.6 #f)))) (x . #f))
;;;
;;; assuming nothing else is in the substitution before the flip
;;; random primitive is run.

;; create a gensym
(define *rp-ls-var* (var 'rp-ls))

(define ext-rp-ls
  (lambda (x rp-call)
    (lambdag@ (p)
      (let ((s (p->s p)))        
        (let ((rp-ls (walk *rp-ls-var* s)))
          (let ((s (remp (lambda (pr) (eq? (car pr) *rp-ls-var*)) s)))
            (let ((rp-ls (if (list? rp-ls) rp-ls '())))
              (let ((rp-ls (cons (cons x rp-call) rp-ls)))
                (let ((s (ext-s *rp-ls-var* rp-ls s)))
                  (let ((p (update-p-with-s p s)))
                    (unit p)))))))))))


;; (define uniform
;;   (lambda (lo hi x)
;;     (delayed-goal `(,lo ,hi)
;;       (project (lo hi x)
;;         (let ((samp (+ (random (- hi lo)) lo)))
;;           (== x samp))))))

;; Our earlier uniform, that doesn't keep track of an rp list
;;
;; (define uniform
;;   (lambda (lo hi x)
;;     (delayed-goal `(,lo ,hi)
;;       (project (lo hi x)
;;         (if (ground? x)
;;             succeed
;;             (let ((samp (+ (random (- hi lo)) lo)))
;;               (== x samp)))))))

;; extend the special rp-ls
(define uniform
  (lambda (lo hi x)
    (delayed-goal `(,lo ,hi)
      (project (lo hi x)
        (if (ground? x)
            succeed
            (let ((samp (+ (random (- hi lo)) lo)))
              (fresh ()
                (== x samp)
                (ext-rp-ls x `(uniform ,lo ,hi ,samp)))))))))

(define uniform-density
  (lambda (x lo hi dx)
    (delayed-goal `(,x ,lo ,hi)
      (project (x lo hi dx)
	(== (if (and (>= x lo) (<= x hi))
		(log (/ (- hi lo)))
		(log 0.0))
	    dx)))))

;; (define flip
;;   (lambda (p x)
;;     (delayed-goal p
;;       (project (p x)
;;         (let ((samp (random 1.0)))
;;           (== (<= samp p) x))))))

;; Our earlier flip, that doesn't keep track of an rp list
;;
;; (define flip
;;   (lambda (p x)
;;     (delayed-goal p
;;       (project (p x)
;;         (if (ground? x)
;;             succeed
;;             (let ((samp (random 1.0)))
;;               (== (<= samp p) x)))))))

;; extend the special rp-ls
(define flip
  (lambda (p x)
    (delayed-goal p
      (project (p x)
        (if (ground? x)
            succeed
            (let ((samp (random 1.0)))
              (let ((b (<= samp p)))
                (fresh ()
                  (== b x)
                  (ext-rp-ls x `(flip ,p ,b))))))))))

(define flip-density
  (lambda (x p dx)
    (delayed-goal `(,x ,p)
      (project (x p dx)
	(== (log (if x p (- 1 p)))
	    dx)))))

;; (define normal
;;   (lambda (mu sd x)
;;     (delayed-goal `(,mu ,sd)
;;       (project (mu sd x)
;;         (begin
;;           (when (< sd 0)
;;             (error 'normal "given invalid parameters"))
;;           (== (+ mu (* sd (car (marsaglia)))) x))))))

;; Our earlier normal, that doesn't keep track of an rp list
;;
;; (define normal
;;   (lambda (mu sd x)
;;     (delayed-goal `(,mu ,sd)
;;       (project (mu sd x)
;;         (begin
;;           (when (< sd 0)
;;             (error 'normal "given invalid parameters"))
;;           (if (ground? x)
;;               succeed
;;               (== (+ mu (* sd (car (marsaglia)))) x)))))))

;; extend the special rp-ls
(define normal
  (lambda (mu sd x)
    (delayed-goal `(,mu ,sd)
      (project (mu sd x)
        (begin
          (when (< sd 0)
            (error 'normal "given invalid parameters"))
          (if (ground? x)
              succeed
              (let ((val (+ mu (* sd (car (marsaglia))))))
                (fresh ()
                  (== val x)                  
                  (ext-rp-ls x `(normal ,mu ,sd ,val))))))))))

(define normal-density
  (lambda (x mu sd dx)
    (delayed-goal `(,x ,mu ,sd)
      (project (x mu sd dx)
        (let ((sq (lambda (x) (* x x))))
          (let ((tau (/ (sq sd)))
                (pi 3.141592653589793))
            (== (/ (+ (* (- tau) (sq (- x mu)))
                      (log (/ tau pi 2))) 2)
                dx)))))))


(define uniform-sample
  (lambda (lo hi)
    (let ((samp (+ (random (- hi lo)) lo)))
      samp)))

(define marsaglia
  (lambda ()
    (let ((x (uniform-sample -1.0 1.0))
          (y (uniform-sample -1.0 1.0)))
      (let ((s (+ (* x x) (* y y))))
        (let ((q (sqrt (* -2.0 (log s) (/ s)))))
          (cond
            [(and (>= 1 s) (> s 0))
             (cons (* x q) (* y q))]
            [else (marsaglia)]))))))




(define pluso
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (+ x y) z)))))

(define minuso
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (- x y) z)))))

(define *o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (* x y) z)))))

(define /o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (/ x y) z)))))

(define >o
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (> x y) z)))))

(define mino
  (lambda (x y z)
    (delayed-goal `(,x ,y)
      (project (x y z)
        (== (min x y) z)))))
