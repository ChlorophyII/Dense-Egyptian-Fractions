(load "math.so")
(load "ext.so")
(load "efrac-branch.so")

					;(compile-file "primes.scm")
                                        ;(compile-file "math.scm")
                                        ;(compile-file "ext.scm")
                                        ;(compile-file "efrac-branch.scm")
                                        ;(compile-file "efrac.scm")
                                        ;(load "efrac.so")

(define (kill-ps-br br n)
  (define (test y bound)
    (let ([difference (+ (rec-sum y) bound)])
      (cond [(< difference 0) #f]
            [(integer? difference) #t]
            [else (not (divide? n (denominator difference)))])))
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

(define (kill-n-br br n)
  (define (kill-test raw-br-counter-part-x rec-sum-MC)
    (let ([difference (- (+ rec-sum-MC (car raw-br-counter-part-x))
                         (br-r br))])
      (and (not (divide? n (denominator difference)))
           (not (< difference 0)))))
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

(define (kill-br br)
  (let ([n (greatest-prime-power
	    (factor
	     (denominator (br-diff br))))])
    (if (> (gcd (denominator (br-r br)) n) 1)
	(kill-ps-br br n)
	(kill-n-br br n))))
;(define kill-br kill-ps-br)

(define (efrac-representation-br D r)
  (define (recursion collector BRs)
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
                       (kill-br br)])
                  (for-each
                   (lambda (sub-br)
                     (let ([difference (br-diff sub-br)])
                       (cond [(and (integer? difference)
                                   (not (member sub-br collector)))
                              (set! collector (cons sub-br collector))]
                             [(and (not (integer? difference))
                                   (not (member sub-br new-BRs)))
                              (set! new-BRs (cons sub-br new-BRs))])))
                   sub-BRs)))
              BRs)
             (recursion collector new-BRs))]))
  (if (integer? (- (rec-sum D) r))
      (list D)
      (recursion '() (list (make-br D r)))))

(define efrac efrac-representation-br)
                                        ;1, 6, 24, 65, 184,

;;(load "efrac.scm")
;;(time (efrac (range 1 184) 5))
;;(time (efrac (range 1 65) 4))
