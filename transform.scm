(load "match.scm")
(load "utils.scm")
(load "test-check.scm")

;;; Transformation: quoted expression to quoted expression

;;; Density transformation

;;; We want to turn this:

;; (define prog2
;;   (lambda (x q)
;;     (fresh ()
;;       (normal 0.0 1.0 x)
;;       (normal x 1.0 q))))

;;; into this:

;; (define prog2-density
;;   (lambda (total-density vars)
;;     (fresh (x q)
;;       (== (list x q) vars)
;;       (fresh (dx dq)
;;         ;; original program body, with rp's changed to density relations
;;         (fresh ()
;;           (normal-density 0.0 1.0 x dx)
;;           (normal-density x 1.0 q dq))
;;         ;;
;;         (sumo (list dx dq) total-density)))))
