(define g-job-queue)
(define g-collector)
(define g-controller)
(define die?)

(define-record-type controller
  (fields
   (immutable limit)
   (mutable hungry)
   (immutable mutex))
  (protocol
   (lambda (new)
     (lambda (limit)
       (new limit 0 (make-mutex))))))

(define signal-last-thread
  (lambda ()
    (system "sleep 0.1");TODO: may replace 1 by 0.1
    (condition-signal (efq-ready g-job-queue))))

(define hungry+1!
  (lambda ()
    (let ([c g-controller])
      (with-mutex (controller-mutex c)
		  (controller-hungry-set!
		   c
		   (inc1 (controller-hungry c)))
		  (cond [(= (controller-hungry c)
			    (controller-limit c))
			 (set! die? #t)
			 "time to die\n"
			 (condition-signal
			  (efq-ready g-job-queue))
			 (fork-thread signal-last-thread)])))))

(define hungry-1!
  (lambda ()
    (let ([c g-controller])
      (with-mutex (controller-mutex c)
		  (controller-hungry-set!
		   c
		   (dec1 (controller-hungry c)))))))

(define-record-type efq
  (fields
   (mutable BRs)
   (mutable length)
   (immutable mutex)
   (immutable ready))
  (protocol
   (lambda (new)
     (lambda (branch)
       (if branch
	   (new (list branch) 1 (make-mutex) (make-condition))
	   (new '() 0 (make-mutex) (make-condition)))))))

(define efq-dequeue!
  (lambda (q)
    (with-mutex (efq-mutex q)
		(let loop ()
		  (cond
		   [(= (efq-length q) 0)
		    (hungry+1!)
		    (condition-wait (efq-ready q) (efq-mutex q))
		    (hungry-1!)
		    (cond [die?
			   (condition-signal (efq-ready q))
			   #f]
			  [else (loop)])]
		   [else
		    (let ([br (car (efq-BRs q))])
		      (efq-BRs-set! q (cdr (efq-BRs q)))
		      (efq-length-set! q (dec1 (efq-length q)))
		      br)])))))

(define efq-enqueue!
  (lambda (BRs q)
    (with-mutex (efq-mutex q)
		(efq-BRs-set!
		 q
		 ;; (sort (lambda (br1 br2)
		 ;; 	 (> (br-rec-sum br1)
		 ;; 	    (br-rec-sum br2)))
		 ;;       (append BRs (efq-BRs q))))
		 (merge (lambda (br1 br2)
			  (> (br-rec-sum br1)
			     (br-rec-sum br2)))
			BRs (efq-BRs q)));;BRs and efq-BRs must be sorted
		(efq-length-set!
		 q
		 (+ (efq-length q) (length BRs)))
		(condition-broadcast (efq-ready q)))))

(define make-killer
  (lambda (n)
    (rec killer
	 (lambda ()
	   (if die?
	       (printf "killer ~s dying\n" n)
	       (let ([br (efq-dequeue! g-job-queue)])
		 (if br
		     (let* ([sub-BRs (kill br)]
			    [SOLs (filter (lambda (br)
					    (integer? (br-diff br)))
					  sub-BRs)])
		       (cond [(> (length SOLs) 0)
			      (efq-enqueue! SOLs g-collector)
			      (printf "~s solutions found\n" (length SOLs))
			      (let ([new-BRs
				     (filter (lambda (br)
					       (not (integer? (br-diff br))))
					     sub-BRs)])
				(efq-enqueue! new-BRs g-job-queue))]
			     [else
			      (efq-enqueue! sub-BRs g-job-queue)])
		       (killer))
		     (printf "killer ~s dying\n" n))))))))

(define (efrac-m D r nk)
  (set! die? #f)
  (printf "die? is set as ~s\n" die?)
  (set! g-controller (make-controller nk))
  (printf "controller-limit ~s\n" (controller-limit g-controller))
  (set! g-job-queue (make-efq (make-br D r)))
  (set! g-collector (make-efq #f))
  (do ([nk nk (- nk 1)])
      ((<= nk 0))
    (fork-thread (make-killer nk)))
  (printf "finished scheduling\n"))

;;(load "efq.scm")
