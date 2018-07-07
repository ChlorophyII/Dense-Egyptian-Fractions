(load "math.so")
(load "ext.so")
(load "efrac-branch.so")

					;(compile-file "primes.scm")
                                        ;(compile-file "math.scm")
                                        ;(compile-file "ext.scm")
                                        ;(compile-file "efrac-branch.scm")
                                        ;(compile-file "efrac.scm")
                                        ;(load "efrac.so")

(define (kill-ps-br branch r n)
  (define (test x bound)
    (let ([difference (+ (rec-sum x) bound)])
      (cond [(< difference 0) #f]
            [(integer? difference) #t]
            [else (not (divide? n (denominator difference)))])))
  (let* ([M (filter (lambda (x) (divide? n x)) (br-denominators branch))]
	 [NM (filter (lambda (x) (not (divide? n x))) (br-denominators branch))]
	 [sum-NM (- (br-sum branch) (sum M))]
	 [rec-sum-NM (- (br-rec-sum branch) (rec-sum M))]
	 [power-set-M (power-set M)]
	 [bound (- rec-sum-NM r)])
    (map (lambda (x)
	   (make-br (+ sum-NM (sum x))
		    (+ rec-sum-NM (rec-sum x))
		    (merge < NM x)))
	 (filter (lambda (y) (test y bound))
		 power-set-M))))

(define (kill-n-br branch r n)
  (define (kill-test raw-branch-counter-part-x rec-sum-MC)
    (let ([difference (- (+ rec-sum-MC (car raw-branch-counter-part-x))
                         r)])
      (and (not (divide? n (denominator difference)))
           (not (< difference 0)))))
  (define (append-e e rest)
    (map (lambda (x)
           (merge < x (list e)))
         rest))
  (define (recur N aim sum-r);N = non-zero-pairs
    (cond [(= aim 0) '(())]
          [(or (null? N) (< aim 0) (< sum-r 0))
           '()]
          [else
           (let ([e1 (caar N)])
             (cond
              [(< (- sum-r e1) aim)
               (append-e (cdar N) (recur (cdr N) (- aim e1) (- sum-r e1)))]
              [else ;(> (sum-r e1) aim)
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
                  (recur-gen-sets-aim power-zero-pairs non-zero-pairs (dec1 i) sum-r p))))
    (let ([i (floor (/ sum-r p))]
          [power-zero-pairs
           (power-set
            (map cdr
                 (filter (lambda (x) (= (car x) 0))
                         A)))]
          [non-zero-pairs (filter (lambda (x) (> (car x) 0)) A)])
      (recur-gen-sets-aim power-zero-pairs non-zero-pairs i sum-r p)))
  (let* ([M (filter (lambda (x) (divide? n x)) (br-denominators branch))]
        [MC (filter (lambda (x) (not (divide? n x)))
                    (br-denominators branch))]
        [p (caar (factor n))]
	[l (apply lcm M)]
	[sum-MC (- (br-sum branch) (sum M))]
	[rec-sum-MC (- (br-rec-sum branch) (rec-sum M))]
	[A (map (lambda (x)
		  (cons (modulo (/ l x) p)
			x))
		M)]
	[sum-r (sum (map car A))]
	[raw-branch-counter-part
	 (map (lambda (x) (cons (rec-sum x) x))
	      (generate-sets A sum-r p))]
	[branch-counter-part
	 (filter (lambda (x) (kill-test x rec-sum-MC))
		 raw-branch-counter-part)])
    (map (lambda (x) (make-br (+ sum-MC (sum (cdr x)))
			      (+ rec-sum-MC (car x))
			      (merge < MC (cdr x))))
	 branch-counter-part)))

(define (kill-br branch r n)
  (if (> (gcd (denominator r) n) 1)
      (kill-ps-br branch r n)
      (kill-n-br branch r n)))

(define (efrac-representation-br D r)
  (define (recursion collector branches)
    (cond [(not (= (length branches) 1))
           (display "----------------------\ncollector    branches\n")
           (display (length collector))
           (display "            ")
           (display (length branches))
           (newline)])
    (cond [(null? branches) (map br-denominators collector)]
          [else
           (let ([new-branches '()])
             (for-each
              (lambda (branch)
                (let ([sub-branches
                       (kill-br branch
                                r
                                (greatest-prime-power
                                 (factor
                                  (denominator
                                   (- (br-rec-sum branch)
                                      r)))))])
                  (for-each
                   (lambda (sub-branch)
                     (let ([difference (- (br-rec-sum sub-branch) r)])
                       (cond [(and (integer? difference)
                                   (not (member sub-branch collector)))
                              (set! collector (cons sub-branch collector))]
                             [(and (not (integer? difference))
                                   (not (member sub-branch new-branches)))
                              (set! new-branches (cons sub-branch new-branches))])))
                   sub-branches)))
              branches)
             (recursion collector new-branches))]))
  (if (integer? (- (rec-sum D) r))
      (list D)
      (recursion '() (list (make-br D)))))

(define efrac efrac-representation-br)
                                        ;1, 6, 24, 65, 184,

