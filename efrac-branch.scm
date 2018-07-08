(define-record-type br
  (fields
   (immutable sum)
   (immutable rec-sum)
   (immutable r)
   (immutable diff)
   (immutable denoms))
  (protocol
   (lambda (new)
     (case-lambda [(denoms r)
		   (new (sum denoms)
			(rec-sum denoms)
			r
			(- (rec-sum denoms) r)
			denoms)]
		  [(sum rec-sum r denoms)
		   (new sum
			rec-sum
			r
			(- rec-sum r)
			denoms)]))))

;; (define make-br
;;   (case-lambda
;;    ((D) (cons (sum D)
;;               (cons (rec-sum D)
;;                     D)))
;;    ((sum-br rec-sum-br denominators-br)
;;     (cons sum-br
;;           (cons rec-sum-br
;;                 denominators-br)))))

;; (define br-sum car)
;; (define br-rec-sum cadr)
;; (define br-denominators cddr)
;; (define set-sum! set-car!)
;; (define set-rec-sum! set-cadr!)
