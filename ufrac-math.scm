(guard (x (load "ufrac-primes.so"))
       (load "ufrac-primes.scm"))

(define id (lambda (x) x))

(define reciprocal
  (lambda (x)
    (if (= x 0)
        (error
         '<procedure-RECIPROCAL>
         "\nReciprocal of 0 is undefined.")
        (/ 1 x))))

(define inc1 (lambda (x) (+ x 1)))
(define dec1 (lambda (x) (- x 1)))

(define square (lambda (x) (* x x)))

(define (divide? a b)
  (zero? (modulo b a)))

(define range
  ;; (range 3) gives (0 1 2)
  ;; (range 2 4) gives (2 3 4)
  ;; (range 1 5 2) gives (1 3 5)
  ;; (range 1 5 3) gives (1 4)
  (case-lambda
   [(stop) (iota stop)]
   [(start stop)
    (map (lambda (x)
           (+ x start))
         (iota (inc1 (- stop start))))]
   [(start stop step)
    (map (lambda (x)
           (+ (* x step) start))
         (iota (+ 1 (floor (/ (- stop start) step)))))]))

(define (power-set set)
  ;; The function generates a list of all subsets of set.
  ;; The first subset in the list is always '().
  ;; The last subset in the list is always set.
  (define (power-set-rec set)
    (if (null? set)
	'(())
	(let ((rest (power-set-rec (cdr set))))
          (append rest
                  (map (lambda (element) (cons (car set) element))
		       rest)))))
  (cond [(> (length set) 27)
         (error
          '<procedure-POWER-SET>
          (string-append "\nThe power set of a set with more than 27 elements"
			 " may require virtual memory.\n"))]
        [(> (length set) 20)
         (printf "<procedure-POWER-SET>:\nThe set has cardinality ~s. "
		 (length set))
	 (printf "This may slow down the computation.\n")])
  (power-set-rec set))

(define (apply-rec op l)
  ;; This procedure is designed particularly for
  ;; summing reciprocals of natural numbers.
  (define (length>=2? l)
    (and (pair? l)
         (not (null? (cdr l)))))
  (define (apply-one-iter ll)
    (if (length>=2? ll)
        (append (list (op (car ll) (cadr ll)))
                (apply-one-iter (cddr ll)))
        ll))
  (define (rec ll)
    (if (length>=2? ll)
        (rec (apply-one-iter ll))
        (car ll)))
  (if (null? l)
      (apply op l)
      (rec l)))

(define sum (lambda (x) (apply + x)))

(define sum-rec (lambda (x) (apply-rec + x)))

(define rec-sum
  (lambda (x)
    (if (null? x)
        0
        (sum-rec (map reciprocal x)))))

(define (highest-power p x)
  ;; This function returns k, where p^k||x.
  ;; In this function, p is allowed to be a composite number.
  (cond [(= p 1)
         (error
          '<procedure-HIGHEST-POWER>
          (format "\nThere is no k such that 1^k = ~s." x))]
        [(> (modulo x p) 0) 0]
        [else (+ 1 (highest-power p (/ x p)))]
	))

(define (expt-mod-p p x a)
  (define (recur a)
    ;; a is expected to be an nonnegative integer.
    (cond [(zero? a) 1]
	  [(odd? a)
	   (modulo (* x (recur (dec1 a))) p)]
	  [else ;; a should be even.
	   (let ([r (recur (/ a 2))])
	     (modulo (* r r) p))]))
  (cond [(not (prime? p))
	 (error
	  '<procedure-EXPT-MOD-P>
	  (string-append "\nThe argument p must be a prime, but "
			 (number->string p)
			 " is not."))]
	[(integer? x)
	 (cond [(zero? a) 1] ; 0^0 is taken to be 1.
	       ;; https://en.wikipedia.org/wiki/Zero_to_the_power_of_zero
	       [(divide? p x) 0]
	       [(negative? a)
		(expt-mod-p p
			    (inverse-mod-p p x)
			    (- a))]
	       [else (recur a)])]
	[else (mod (expt x a) p)]))

(define (inverse-mod-p p x)
  ;; This function returns y, where xy=1 mod p.
  ;; We use Fermat's little theorem.
  (cond [(not (prime? p))
	 (error
	  '<procedure-INVERSE-MOD-P>
	  (string-append "\nThe argument p must be a prime, but "
			 (number->string p)
			 " is not.\n"))]
	[(divide? p x)
	 (error
	  '<procedure-INVERSE-MOD-P>
	  (format "\np = ~s divides x = ~s. There is no inverse of x mod p.\n"
		  p
		  x))]
	[else
	 (expt-mod-p p x (- p 2))]))

(define (factor x)
  (define (factor-rec y primes-list)
    (if (null? primes-list)
        (error
         '<procedure-FACTOR>
         (format "~s = ~s * ~s contains a prime factor that is not in ~s."
		 x
		 (/ x y)
		 y
		 'primes-list)))
    (let ([p (car primes-list)])
      (cond [(= y 1)
	     '()]
	    [(divide? p y)
	     (append (list (list p (highest-power p y)))
		     (factor-rec
		      (/ y (expt p (highest-power p y)))
		      (cdr primes-list)))]
	    [else
	     (factor-rec y (cdr primes-list))])))
  (cond [(or (not (integer? x))
	     (<= x 0))
	 (error
	  '<procedure-FACTOR>
	  (format "Invalid input: ~s" x))]
	[(= x 1) '((1 1))]
	[else (factor-rec x primes-list)]))

(define (kill-p-factor p x)
  (if (integer? (/ x p))
      (kill-p-factor p (/ x p))
      x))

(define (prime-divisors x)
  (map car (factor x)))

(define (greatest-prime-power factorization)
  (apply max
	 (map (lambda (x) (expt (car x) (cadr x)))
	      factorization)))

(define (set-difference s1 s2)
  (cond [(null? s1)
         '()]
        [(not (member (car s1) s2))
         (cons (car s1) (set-difference (cdr s1) s2))]
        [else
         (set-difference (cdr s1) s2)]))

(define (multiset-difference s1 s2)
  (define (recur s1 s2)
    (cond [(null? s1)
           '()]
	  [(null? s2)
	   s1]
          [(= (car s1) (car s2)) (recur (cdr s1) (cdr s2))]
	  [(< (car s1) (car s2)) (cons (car s1) (recur (cdr s1) s2))]
	  [else (recur s1 (cdr s2))]))
  (recur (sort < s1) (sort < s2)))

(define (set-symmetric-difference s1 s2)
  (append (set-difference s1 s2) (set-difference s2 s1)))

(define (set-intersection s1 s2)
  (define (recur s1 s2)
    (cond [(or (null? s1) (null? s2)) '()]
          [(= (car s1) (car s2))
           (cons (car s1)
                 (recur (cdr s1) (cdr s2)))]
          [(< (car s1) (car s2))
           (recur (cdr s1) s2)]
          [else
           (recur s1 (cdr s2))]))
  (recur (sort < s1) (sort < s2)))

(define (sets-intersection sets)
  (if (length>=2? sets)
      (set-intersection (car sets)
                        (sets-intersection (cdr sets)))
      (car sets)))

(define (set-dedupe set)
  (cond [(null? set) '()]
	[else
         (if (member (car set) (cdr set))
             (set-dedupe (cdr set))
             (cons (car set) (set-dedupe (cdr set))))]))

