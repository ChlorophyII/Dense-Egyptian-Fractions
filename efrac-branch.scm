(define-record-type br
  (fields
   (immutable sum)
   (immutable rec-sum)
   (immutable r)
   (immutable diff)
   (immutable gpp);;greatest prime power of denominator of diff
   (immutable denoms))
  (protocol
   (lambda (new)
     (case-lambda [(sum rec-sum r gpp denoms)
		   (new sum
			rec-sum
			r
			(- rec-sum r)
			gpp
			denoms)]
		  [(denoms r)
		   (new (sum denoms)
			(rec-sum denoms)
			r
			(- (rec-sum denoms) r)
			(greatest-prime-power
			 (factor (denominator
				  (- (rec-sum denoms) r))))
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
