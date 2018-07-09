(load "math.so")
(load "ext.so")
(load "efrac-branch.so")
(load "efq.so")
					;(compile-file "primes.scm")
                                        ;(compile-file "math.scm")
                                        ;(compile-file "ext.scm")
                                        ;(compile-file "efrac-branch.scm")
                                        ;(compile-file "efrac.scm")
					;(compile-file "efq.scm")
                                        ;(load "efrac.so")

(define (kill-ps br n)
  (define (test y bound)
    (let ([diff (+ (rec-sum y) bound)])
      (cond [(< diff 0) #f]
            [(integer? diff) #t]
            [else (not (divide? n (denominator diff)))])))
  (let* ([M (filter (lambda (x) (divide? n x)) (br-denoms br))]
					; M: multiples of n
	 [NM (filter (lambda (x) (not (divide? n x))) (br-denoms br))]
	 [sum-NM (- (br-sum br) (sum M))]
	 [rec-sum-NM (- (br-rec-sum br) (rec-sum M))]
	 [power-set-M (power-set M)]
	 [bound (- rec-sum-NM (br-r br))])
    (map (lambda (x)
	   (make-br (+ sum-NM (sum x))
		    (+ rec-sum-NM (rec-sum x))
		    (br-r br)
		    (merge < NM x)))
	 (filter (lambda (y) (test y bound))
		 power-set-M))))

(define (kill-n br n)
  (define (kill-test raw-br-counter-part-x rec-sum-MC)
    (let ([diff (- (+ rec-sum-MC (car raw-br-counter-part-x))
                         (br-r br))])
      (and (not (divide? n (denominator diff)))
           (not (< diff 0)))))
  (define (append-e e rest)
    (map (lambda (x)
           (merge < x (list e)))
         rest))
  (define (recur N aim sum-r)
					; N: non-zero-pairs
    (cond [(= aim 0) '(())]
          [(or (null? N) (< aim 0) (< sum-r 0))
           '()]
          [else
           (let ([e1 (caar N)])
             (cond
              [(< (- sum-r e1) aim)
               (append-e (cdar N)
			 (recur (cdr N) (- aim e1) (- sum-r e1)))]
              [else ; (> (sum-r e1) aim)
               (append (append-e
                        (cdar N)
                        (recur (cdr N) (- aim e1) (- sum-r e1)))
                       (recur (cdr N) aim (- sum-r e1)))]))]))
  (define (generate-sets-aim power-zero-pairs non-zero-pairs aim sum-r)
    (let ([rest (recur non-zero-pairs aim sum-r)])
      (cond [(and (= aim 0)); (null? rest))
             power-zero-pairs]
            [(and (> aim 0) (null? rest))
             '()]
            [else
             (apply append
                    (map (lambda (x) (map (lambda (y) (merge < x y))
                                          power-zero-pairs))
                         rest))])))
  (define (generate-sets A sum-r p)
    (define (recur-gen-sets-aim power-zero-pairs non-zero-pairs i sum-r p)
      (if (< i 0)
          '()
          (append (generate-sets-aim power-zero-pairs
                                     non-zero-pairs
                                     (* i p)
                                     sum-r)
                  (recur-gen-sets-aim power-zero-pairs
				      non-zero-pairs
				      (dec1 i)
				      sum-r
				      p))))
    (let ([i (floor (/ sum-r p))]
          [power-zero-pairs
           (power-set
            (map cdr
                 (filter (lambda (x) (= (car x) 0))
                         A)))]
          [non-zero-pairs (filter (lambda (x) (> (car x) 0)) A)])
      (recur-gen-sets-aim power-zero-pairs non-zero-pairs i sum-r p)))
  (let* ([M (filter (lambda (x) (divide? n x)) (br-denoms br))]
	 [MC (filter (lambda (x) (not (divide? n x)))
		     (br-denoms br))]
	 [p (caar (factor n))]
	 [l (apply lcm M)]
	 [sum-MC (- (br-sum br) (sum M))]
	 [rec-sum-MC (- (br-rec-sum br) (rec-sum M))]
	 [A (map (lambda (x)
		   (cons (modulo (/ l x) p)
			 x))
		 M)]
	 [sum-r (sum (map car A))]
	 [raw-br-counter-part
	  (map (lambda (x) (cons (rec-sum x) x))
	       (generate-sets A sum-r p))]
	 [br-counter-part
	  (filter (lambda (x) (kill-test x rec-sum-MC))
		  raw-br-counter-part)])
    (map (lambda (x) (make-br (+ sum-MC (sum (cdr x)))
			      (+ rec-sum-MC (car x))
			      (br-r br)
			      (merge < MC (cdr x))))
	 br-counter-part)))

(define (kill br)
  (let ([n (greatest-prime-power
	    (factor
	     (denominator (br-diff br))))])
    (if (> (gcd (denominator (br-r br)) n) 1)
	(kill-ps br n)
	(kill-n br n))))
;(define kill kill-ps)

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
                (let ([sub-BRs
                       (kill br)])
                  (for-each
                   (lambda (sub-br)
                     (let ([diff (br-diff sub-br)])
                       (cond [(and (integer? diff)
                                   (not (member sub-br collector)))
                              (set! collector (cons sub-br collector))]
                             [(and (not (integer? diff))
                                   (not (member sub-br new-BRs)))
                              (set! new-BRs (cons sub-br new-BRs))])))
                   sub-BRs)))
              BRs)
             (recur collector new-BRs))]))
  (if (integer? (- (rec-sum D) r))
      (list D)
      (recur '() (list (make-br D r)))))

                                        ;1, 6, 24, 65, 184,




;;(load "efrac.scm")
;;(time (efrac (range 1 184) 5))
;;(time (efrac (range 1 65) 4))
;;(efrac-m (range 1 65) 4 4)
;;(+ 1 1)
;;(efq-BRs g-collector)
;;(efq-BRs g-job-queue)
