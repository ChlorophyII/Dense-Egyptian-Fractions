(define-record-type pool
  (fields
   (mutable items)
   (mutable size)
   (immutable mutex))
  (protocol
   (lambda (new)
     (lambda (items)
       (new items
	    (length items)
	    (make-mutex))))))

(define retrieve-items!
  (lambda (pool)
    (with-mutex (pool-mutex pool)
		(let ([items (pool-items pool)])
		  (cond [(null? items) #f]
			[else
			 (let ([batch-size (min max-batch-size
						(pool-size pool))])
			   (pool-items-set! pool
					    (list-tail items batch-size))
			   (pool-size-set! pool
					   (- (pool-size pool) batch-size))
			   (list-head items batch-size))])))))

(define add-items!
  (lambda (pool items)
    (let ([num-items (length items)])
      (with-mutex (pool-mutex pool)
		  (pool-items-set! pool
				   (append items
					   (pool-items pool)))
		  (pool-size-set! pool
				  (+ num-items
				     (pool-size pool)))))))

(define BRANCHes)
(define SOLUTIONs)

(define suicide?)
(define counter)
(define max-batch-size 20)
(define number-threads 8)

(define make-processor
  ;; a processor is both a producer and a consumer
  (lambda (n)
    (rec processor
	 (lambda ()
	   (cond [suicide?
		  (if verbose?
		      (printf "processor ~s dying\n" n))]
		 [else
		  (if verbose?
		      (printf "processor ~s looking for branches to kill\n" n))
		  (let ([jobs (retrieve-items! BRANCHes)])
		    (cond [jobs
			   (let ([kill-output (map kill jobs)])
			     (if verbose?
				 (printf "processor ~s adding results of killing ~s branches to job-pool\n"
					 n
					 (length jobs)))
			     (add-items! SOLUTIONs
					 (apply append (map car kill-output)))
			     (add-items! BRANCHes
					 (apply append (map cdr kill-output)))
			     ;; (map cadr ...) does not work, as there could be no new branches to compute
			     )
			   (vector-set! counter n (+ (vector-ref counter n)
						     (length jobs)))]))
		  (processor)])))))

(define (ufrac-mt D r)
  (set! BRANCHes (make-pool (list (br-reduce (make-br D r)))))
  (set! SOLUTIONs (make-pool '()))
  (set! suicide? #f)
  (set! counter (make-vector number-threads 0))
  (for-each (lambda (x) (fork-thread (make-processor x)))
	    (range number-threads))
  (let ([num-kill (sum (vector->list counter))])
    (let loop ()
      (sleep (make-time 'time-duration 0 1))
      (printf "solutions: ~s branches: ~s killed branches: ~s ~s\n"
	      (pool-size SOLUTIONs)
	      (pool-size BRANCHes)
	      (sum (vector->list counter))
	      (time-utc->date (current-time)))
      (cond [(and (null? (pool-items BRANCHes))
		  (= num-kill (sum (vector->list counter))))
	     (sleep (make-time 'time-duration 500000000 0))
	     (if (and (null? (pool-items BRANCHes))
		      (= num-kill (sum (vector->list counter))))
		 (set! suicide? #t))]
	    [else
	     (set! num-kill (sum (vector->list counter)))
	     (loop)])))
  (sleep (make-time 'time-duration 10000000 0))
  (map br-denoms-sol (pool-items SOLUTIONs)))

(define (ufrac-mt-es D r)
  (set! BRANCHes (make-pool (list (br-reduce (make-br D r)))))
  (set! SOLUTIONs (make-pool '()))
  (set! suicide? #f)
  (set! counter (make-vector number-threads 0))
  (for-each (lambda (x) (fork-thread (make-processor x)))
	    (range number-threads))
  (let ([num-kill (sum (vector->list counter))])
    (let loop ()
      (sleep (make-time 'time-duration 0 1))
      (printf "solutions: ~s branches: ~s killed branches: ~s ~s\n"
	      (pool-size SOLUTIONs)
	      (pool-size BRANCHes)
	      (sum (vector->list counter))
	      (time-utc->date (current-time)))
      (cond [(positive? (pool-size SOLUTIONs))
	     (set! suicide? #t)]
	    [(and (null? (pool-items BRANCHes))
		  (= num-kill (sum (vector->list counter))))
	     (sleep (make-time 'time-duration 500000000 0))
	     (if (and (null? (pool-items BRANCHes))
		      (= num-kill (sum (vector->list counter))))
		 (set! suicide? #t))]
	    [else
	     (set! num-kill (sum (vector->list counter)))
	     (loop)])))
  (sleep (make-time 'time-duration 10000000 0))
  (if (null? (pool-items SOLUTIONs))
      #f
      (br-denoms-sol (car (pool-items SOLUTIONs)))))
