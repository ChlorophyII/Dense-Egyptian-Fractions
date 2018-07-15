(load "math.scm")
(load "ext.scm")
(load "efrac-branch.scm")
(load "efq.scm")
					;(compile-file "primes.scm")
                                        ;(compile-file "math.scm")
                                        ;(compile-file "ext.scm")
                                        ;(compile-file "efrac-branch.scm")
                                        ;(compile-file "efrac.scm")
					;(compile-file "efq.scm")
                                        ;(load "efrac.so")
(set! verbose? #f)

(define (kill-ps br)
  (let* ([n (br-gpp br)]
	 [M (filter (lambda (x) (divide? n x)) (br-denoms br))]
					; M: multiples of n
	 [NM (remp (lambda (x) (divide? n x)) (br-denoms br))]
	 [sum-NM (- (br-sum br) (sum M))]
	 [rs-NM (- (br-rec-sum br) (rec-sum M))]
	 [rs-SUBSETs-M (map (lambda (subset-M)
			      (cons (rec-sum subset-M) subset-M))
			    (power-set M))];; reciprocal sum of and subsets of M
	 [bound (- rs-NM (br-r br))]
	 [discard? (lambda (x) (or (divide? n (denominator (+ bound (car x))))
				   (< (+ bound (car x)) 0)))]
	 [reduced-rs-SUBSETs-M (remp discard? rs-SUBSETs-M)]
	 [solution? (lambda (x) (integer? (+ bound (car x))))])
    (cons (map (lambda (x) ;; solutions
		 (let ([subset-M (cdr x)])
		   (make-br (+ sum-NM (sum subset-M))
			    (+ rs-NM (car x))
			    (br-r br)
			    1
			    (merge < NM subset-M))))
	       (filter solution? reduced-rs-SUBSETs-M))
	  (map (lambda (x) ;; new-BRs
		 (let ([subset-M (cdr x)])
		   (make-br (+ sum-NM (sum subset-M))
			    (+ rs-NM (car x))
			    (br-r br)
			    (greatest-prime-power
			     (factor (denominator (+ bound (car x)))))
			    (merge < NM subset-M))))
	       (remp solution? reduced-rs-SUBSETs-M)))))

(define (kill-n br)
  (define (knapsack N aim sum-NUMs) ; N: non-zero-pairs
    (define (insert-foreach e collection-LISTs)
      (map (lambda (l)
             (merge < l (list e)))
           collection-LISTs))
    (cond [(= aim 0) '(())]
          [(or (null? N) (< aim 0) (< sum-NUMs 0))
           '()]
          [else
           (let ([num (caar N)])
             (cond
              [(< (- sum-NUMs num) aim)
               (insert-foreach (cdar N)
			 (knapsack (cdr N) (- aim num) (- sum-NUMs num)))]
              [else ; (> (sum-NUMs num) aim)
               (append (insert-foreach
                        (cdar N)
                        (knapsack (cdr N) (- aim num) (- sum-NUMs num)))
                       (knapsack (cdr N) aim (- sum-NUMs num)))]))]))
  (define (generate-sets-aim ps-0-num-M non-zero-pairs aim sum-NUMs-M)
    (let ([rest (knapsack non-zero-pairs aim sum-NUMs-M)])
      (cond [(= aim 0) ps-0-num-M]
            [else
             (apply append
                    (map (lambda (x)
			   (map (lambda (y) (merge < x y)) ps-0-num-M))
                         rest))]))); TODO optimize it
  (define (generate-sets A sum-NUMs-M p)
    (define (recur-gen-sets-aim ps-0-num-M non-zero-pairs i sum-NUMs-M p)
      (if (< i 0)
          '()
          (append (generate-sets-aim ps-0-num-M
                                     non-zero-pairs
                                     (* i p)
                                     sum-NUMs-M)
                  (recur-gen-sets-aim ps-0-num-M
				      non-zero-pairs
				      (dec1 i)
				      sum-NUMs-M
				      p))))
    (let ([i (floor (/ sum-NUMs-M p))]
          [ps-0-num-M
           (power-set
            (map cdr
		 (filter (lambda (x) (= (car x) 0)) A)))]
          [non-zero-pairs (filter (lambda (x) (> (car x) 0)) A)])
      (recur-gen-sets-aim ps-0-num-M non-zero-pairs i sum-NUMs-M p)))
  (let* ([n (br-gpp br)]
	 [M (filter (lambda (x) (divide? n x)) (br-denoms br))]
	 [NM (remp (lambda (x) (divide? n x)) (br-denoms br))]
	 [sum-NM (- (br-sum br) (sum M))]
	 [rs-NM (- (br-rec-sum br) (rec-sum M))]
	 [bound (- rs-NM (br-r br))]
	 [discard? (lambda (x) (or (divide? n (denominator (+ bound (car x))))
	  			   (< (+ bound (car x)) 0)))]; TODO optimize it
	 [p (caar (factor n))]; TODO cannot we directly use n?
	 [l (apply lcm M)]
	 [A (map (lambda (x)
		   (cons (modulo (/ l x) p); TODO modulo p or n?; 
			 x))
		 M)]
	 [sum-NUMs-M (sum (map car A))]
	 [reduced-rs-SUBSETs-M
	  (remp discard? (map (lambda (x) (cons (rec-sum x) x))
			      (generate-sets A sum-NUMs-M p)))]
	 [solution? (lambda (x) (integer? (+ bound (car x))))])
    (cons (map (lambda (x) ;; solutions
		 (let ([subset-M (cdr x)])
		   (make-br (+ sum-NM (sum subset-M))
			    (+ rs-NM (car x))
			    (br-r br)
			    1
			    (merge < NM subset-M))))
	       (filter solution? reduced-rs-SUBSETs-M))
	  (map (lambda (x) ;; new-BRs
		 (let ([subset-M (cdr x)])
		   (make-br (+ sum-NM (sum subset-M))
			    (+ rs-NM (car x))
			    (br-r br)
			    (greatest-prime-power
			     (factor (denominator (+ bound (car x)))))
			    (merge < NM subset-M))))
	       (remp solution? reduced-rs-SUBSETs-M)))))

(define (kill br)
  (let ([n (br-gpp br)])
    (if verbose?
	(printf "`kill` branch size:~s n:~s\n" (length (br-denoms br)) n))
    (if (> (gcd (denominator (br-r br)) n) 1)
	(kill-ps br)
	(kill-n br))))
(define kill kill-ps)

(define (efrac D r)
  (define (recur collector BRs)
    (cond [(not (= (length BRs) 1))
           (display "----------------------\ncollector    branches\n")
	   (printf "~s            ~s\n"
		   (length collector)
		   (length BRs))])
    (cond [(null? BRs) (map br-denoms collector)]
          [else
           (let ([new-BRs '()])
             (for-each
              (lambda (br)
                (let ([SOLs-BRs
                       (kill br)])
		  (set! collector (append collector (car SOLs-BRs)))
		  (set! new-BRs (append new-BRs (cdr SOLs-BRs)))))
              ;;      (for-each
              ;;       (lambda (sub-br)
              ;;         (let ([diff (br-diff sub-br)])
              ;;           (cond [(and (integer? diff)
              ;;                       (not (member sub-br collector)))
              ;;                  (set! collector (cons sub-br collector))]
              ;;                 [(and (not (integer? diff))
              ;;                       (not (member sub-br new-BRs)))
              ;;                  (set! new-BRs (cons sub-br new-BRs))])))
              ;;       sub-BRs)))
              BRs)
             (recur collector new-BRs))]))
  (if (integer? (- (rec-sum D) r))
      (list D)
      (recur '() (list (make-br D r)))))

                                        ;1, 6, 24, 65, 184,




;;(load "efrac.scm")
;;(time (efrac (range 1 184) 5))
;;(time (efrac (range 1 65) 4))
;;(efrac (range 1 24) 3)
;;(efrac-m (range 1 65) 4 4)
;;(+ 1 1)
;;(efrac-m (range 1 184) 5 1)
;;(efq-BRs g-collector)
;;(length (efq-BRs g-job-queue))
;;(controller-limit g-controller)
;;(controller-hungry g-controller)
;;(greatest-prime-power '())
;;(efrac (range 1 6) 2)
