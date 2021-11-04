(guard (x (load "ufrac-math.so")
	  (load "ufrac-branch.so")
	  (load "ufrac-mt.so")
	  (load "ufrac-conjecture.so"))
       (load "ufrac-math.scm")
       (load "ufrac-branch.scm")
       (load "ufrac-mt.scm")
       (load "ufrac-conjecture.scm"))

(set! verbose? #f)
(set! print-frequency 1000)

(define (kill br)
  (define (knapsack A aim sum-NUMs)
    (define (for-each-insert e LISTs)
      (map (lambda (l)
             (append l (list e)))
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
		  ;; since the order of numbers in (br-denoms br) is preserved,
		  ;; d is always the least number
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
			 (remp (lambda (br) (or (negative? (br-r br))
						(negative? (br-diff br))))
			       ;; remove impossible branches produced by br-reduce
			  (map (lambda (x) ; new-BRs
				 (let ([subset-M (cdr x)])
				   (br-reduce (make-br sum-NM
						       rs-NM
						       rs-M
						       subset-M
						       NM
						       br))))
			       (remp solution? reduced-SUBSETs-M)))))))])))

(define (ufrac D r)
  (define SOLUTIONs '())
  (define (distinct-denoms) #f)
  (define (recur BRs n)
    (cond [(null? BRs) '()]
	  [else
	   (let* ([SOLs-BRs (kill (car BRs))]
		  [new-BRs (cdr SOLs-BRs)]
		  [next-BRs (append new-BRs (cdr BRs))] ; order is important
		  [n+1 (add1 n)])
	     (if distinct-denoms
		 (set! SOLUTIONs (append (car SOLs-BRs) SOLUTIONs))
		 (for-each ; for each new sol
		  (lambda (new-sol)
		    (if (null?
			 (filter
			  (lambda (old-sol) (br-equal-as-sol? new-sol old-sol))
			  SOLUTIONs)) ; consider (ufrac '(2 2 2 2) 1)
			(set! SOLUTIONs (cons new-sol SOLUTIONs))))
		  (car SOLs-BRs)))
	     (cond [(and verbose? (divide? print-frequency n+1))
		    (printf "killed branches: ~12s  branches: ~12s  solutions: ~10s  ~s \n"
			    n+1
			    (length BRs)
			    (length SOLUTIONs)
			    (time-utc->date (current-time)))
		    (cond [(not (null? next-BRs))
			   (br-display (car next-BRs))
			   (newline)])])
	     (recur next-BRs (add1 n)))]))
  (let ([first-br (br-reduce (make-br D r))])
    (set! distinct-denoms (set-distinct-numbers? (br-denoms first-br)))
    (recur (list first-br) 0))
  (if verbose?
      (printf "Found ~s representations of ~s.\n"
	      (length SOLUTIONs)
	      r))
  (map br-denoms-sol SOLUTIONs))

(define (ufrac-es D r)
  (define (recur BRs n)
    (cond [(null? BRs) '()]
	  [else
	   (let* ([SOLs-BRs (kill (car BRs))]
		  [SOLs (car SOLs-BRs)]
		  [new-BRs (cdr SOLs-BRs)])
	     (cond [(null? SOLs)
		    (let ([next-BRs (append new-BRs (cdr BRs))]
			  [n+1 (add1 n)])
		      (cond [(and verbose? (divide? print-frequency n+1))
			     (printf "killed branches: ~12s  branches: ~12s  ~s \n"
				     n+1
				     (length BRs)
				     (time-utc->date (current-time)))
			     (cond [(not (null? next-BRs))
				    (br-display (car next-BRs))
				    (newline)])])
		      (recur next-BRs (add1 n)))]
		   [else
		    (map br-denoms-sol (list (car SOLs)))]))]))
  (let ([sol (recur (list (br-reduce (make-br D r))) 0)])
    (if (null? sol)
	#f
	(car sol))))

(define (ufrac-es-progress D r)
  (define sol '()) 
  (define progress 0)
  (define num-killed-brs 0)
  (define (dig br num-BRs-above treasure-map)
    (cond [(null? sol)
	   (let* ([SOLs-BRs (kill br)]
		  [SOLs (car SOLs-BRs)]
		  [new-BRs (cdr SOLs-BRs)]
		  [length-new-BRs (length new-BRs)])
	     (set! num-killed-brs (add1 num-killed-brs))
	     (let ([num-BRs (* num-BRs-above length-new-BRs)]
		   [new-treasure-map (if (> length-new-BRs 1)
					 (cons length-new-BRs treasure-map)
					 treasure-map)])
	       (cond [(null? SOLs)
		      (for-each (lambda (br) (dig br num-BRs new-treasure-map))
				new-BRs)
		      (cond [(and verbose? (null? new-BRs))
			     (set! progress (+ progress
					       (reciprocal num-BRs-above)))
			     (printf "progress: ~14,10f%  killed branches: ~12s  ~s ~s\n"
				     (* 100 progress)
				     num-killed-brs
				     (time-utc->date (current-time))
				     new-treasure-map)])]
		     [else (set! sol (map br-denoms-sol (list (car SOLs))))]
		     )))]
	  ))
  (dig (br-reduce (make-br D r)) 1 '())
  (if (null? sol)
	#f
	(car sol)))

(define (ufrac-bfs D r)
  (define (distinct-denoms) #f)
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
	     (if distinct-denoms
		 (set! SOLUTIONs (append (car SOLs-BRs) SOLUTIONs))
		 (for-each ; for each new sol
		  (lambda (new-sol)
		    (if (null?
			 (filter
			  (lambda (old-sol) (br-equal-as-sol? new-sol old-sol))
			  SOLUTIONs)) ; consider (ufrac-bfs '(2 2 2 2) 1)
			(set! SOLUTIONs (cons new-sol SOLUTIONs))))
		  (car SOLs-BRs)))
	     (for-each ; for each new br
	      (lambda (new-br)
		(set! new-BRs (cons new-br new-BRs)))
	      (cdr SOLs-BRs))))
	 BRs)
	(recur SOLUTIONs new-BRs))]))
  (let ([first-br (br-reduce (make-br D r))])
    (set! distinct-denoms (set-distinct-numbers? (br-denoms first-br)))
    (recur '() (list first-br))))
