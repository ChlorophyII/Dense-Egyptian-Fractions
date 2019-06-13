(load "math.so")
(load "efrac-branch.so")
;; (compile-file "primes.scm")
;; (compile-file "math.scm")
;; (compile-file "efrac-branch.scm")
;; (compile-file "efrac.scm")
;; (load "efrac.scm")
(set! verbose? #f)

(define (kill br)
  (define (knapsack A aim sum-NUMs)
    (define (for-each-insert e LISTs)
      (map (lambda (l)
             (merge < l (list e)))
	   LISTs))
    (cond [(zero? aim) '(())] ; There is a solution
	  [(or (null? A) (negative? aim)) '()] ; There is no solution
	  [else
	   (let ([num (caar A)])
	     (cond [(< (- sum-NUMs num) aim)
		    (for-each-insert (cdar A)
				     (knapsack (cdr A)
					       (- aim num)
					       (- sum-NUMs num)))]
		   [else ; (>= (- sum-NUMs num) aim)
		    (append (for-each-insert (cdar A)
					     (knapsack (cdr A)
						       (- aim num)
						       (- sum-NUMs num)))
			    (knapsack (cdr A) aim (- sum-NUMs num)))]
		   ))]
	  ))
  (define (generate-sets-aim-mod A aim-mod p)
    (let ([sum-NUMs (sum (map car A))])
      (apply append
	     (map (lambda (aim) (knapsack A aim sum-NUMs))
		  (range aim-mod sum-NUMs p)))))
  (define (compute-p^highest-power p br)
    (let ([M-p (filter (lambda (x) (divide? p x)) (br-denoms br))])
      (if (null? M-p)
	  #f
	  (expt p (apply max (map (lambda (x) (highest-power p x)) M-p))))))
  (define (compute-pre-aim p p^highest-power br)
    (let* ([denom-diff (denominator (br-diff br))]
	   [p^highest-power-denom-diff (/ denom-diff (kill-p-factor p denom-diff))])
      (cond [(not p^highest-power) #f] ; Either p^highest-power = #f
	    [(= p^highest-power-denom-diff p^highest-power)
	     (* (numerator (br-diff br))
		(inverse-mod-p p
			       (kill-p-factor p
					      (denominator (br-diff br)))))]
	    [(< p^highest-power-denom-diff p^highest-power) 0]
	    [else #f] ;p^highest-power-denom-diff > p^highest-power
	    )))
  (let ([r (br-r br)]
	[diff (br-diff br)])
    (cond [(br-denoms-sol br) (cons (list br) '())] ; br is actually a solution
	  [(or (negative? r) (negative? diff)) '(())]
	  [(integer? diff)
	   (let* ([d (car (br-denoms br))]
		  [rec-d (/ 1 d)])
	     (cond [(< r rec-d) (cons '() (list (br-discard-first-d br)))]
		   [(= r rec-d) (cons (list (br-reserve-first-d br))
				      (list (br-discard-first-d br)))]
		   [else ; r > rec-d
		    (cons '() (list (br-reserve-first-d br)
				    (br-discard-first-d br)))]
		   ))]
	  [else
	   (let* 
	       ([p (caar (factor (br-gpp br)))]
		[p^highest-power (compute-p^highest-power p br)]
		[pre-aim (compute-pre-aim p p^highest-power br)])
	     (if (not pre-aim) ; Consider the example (efrac '(2 2 4 4) 9/8).
		 '(())
		 (let*
		     ([M (filter (lambda (x) (divide? p^highest-power x))
				 (br-denoms br))]
		      [NM (remp (lambda (x) (divide? p^highest-power x))
				(br-denoms br))]
		      [rs-M (rec-sum M)]
		      [sum-NM (- (br-sum-denoms br) (sum M))]
		      [rs-NM (- (br-rs-denoms br) rs-M)]
		      [bound (- rs-NM (br-r br))]
		      [discard? (lambda (x) (or (negative? (+ bound (car x)))
						(> (car x) (br-r br))))]
		      [solution? (lambda (x) (zero? (+ bound (car x))))]
		      [A (map (lambda (x)
				(cons (inverse-mod-p p (/ x p^highest-power))
				      x))
			      M)]
		      [aim-mod (modulo (- (sum (map car A)) pre-aim) p)]
		      [reduced-SUBSETs-M
		       (remp discard?
			     (map (lambda (x) (cons (rec-sum x) x))
				  (generate-sets-aim-mod A aim-mod p)))])
		   (cons (map (lambda (x) ; solutions
				(let ([subset-M (cdr x)])
				  (make-br sum-NM
					   rs-NM
					   rs-M
					   subset-M
					   NM
					   br)))
			      (filter solution? reduced-SUBSETs-M))
			 (map (lambda (x) ; new-BRs
				(let ([subset-M (cdr x)])
				  (make-br sum-NM
					   rs-NM
					   rs-M
					   subset-M
					   NM
					   br)))
			      (remp solution? reduced-SUBSETs-M))))))])))

(define (efrac D r)
  (define (recur SOLUTIONs BRs)
    (cond [(and verbose? (not (= (length BRs) 1)))
	   (printf "~10@s solutions ~10@s branches  ~s\n"
		   (length SOLUTIONs)
		   (length BRs)
		   (time-utc->date (current-time)))])
    (cond ;[(not (null? SOLUTIONs)) (map br-denoms-sol SOLUTIONs)]
     [(null? BRs) (map br-denoms-sol SOLUTIONs)]
     [else
      (let ([new-BRs '()])
	(for-each ; for each br in BRs
	 (lambda (br)
	   (let ([SOLs-BRs (kill br)])
	     (for-each ; for each new sol
	      (lambda (new-sol)
		(if (null?
		     (filter
		      (lambda (old-sol) (br-equal-as-sol? new-sol old-sol))
		      SOLUTIONs))
		    (set! SOLUTIONs (cons new-sol SOLUTIONs))))
	      (car SOLs-BRs))
	     (for-each ; for each new br
	      (lambda (new-br)
		(set! new-BRs (cons new-br new-BRs)))
	      (cdr SOLs-BRs))))
	 BRs)
	(recur SOLUTIONs new-BRs))]))
  (recur '() (list (make-br D r))))

                                        ;1, 6, 24, 65, 184,




;;(load "efrac.scm")
;;(time (set! a (efrac (range 1 24) 3)))
;;(time (set! b (efrac (range 1 65) 4)))
;;(time (efrac (range 1 183) 5))
;;(time (set! c (efrac (range 1 184) 5)))
;;(time (set! d (efrac (range 1 35) 2/7)))
;;(time (efrac (range 1 468) 6))
