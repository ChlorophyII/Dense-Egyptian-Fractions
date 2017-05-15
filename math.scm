(load "primes.so")

(define id (lambda (x) x))

(define reciprocal
  (lambda (x)
    (if (= x 0)
        (error
         '<procedure-RECIPROCAL>
         "\nReciprocal of 0 is undefined")
        (/ 1 x))))

(define inc1 (lambda (x) (+ x 1)))
(define dec1 (lambda (x) (- x 1)))

(define square (lambda (x) (* x x)))

(define (divide? a b)
  (= (modulo b a) 0))

(define range
  ;;(range 3) gives (0 1 2)
  ;;(range 2 4) gives (2 3 4)
  ;;(range 1 5 2) gives (1 3)
  (case-lambda
   ((stop) (iota stop))
   ((start stop)
    (map (lambda (x)
           (+ x start))
         (iota (inc1 (- stop start)))))
   ((start stop step)
    (map (lambda (x)
           (+ (* x step) start))
         (iota (round (/ (- stop start) step)))))))

(define (power-set set)
  ;;The function generates a list of all subsets of 'set'
  ;;The first subset in the list is always '()
  ;;The last subset in the list is always 'set'
  (define (power-set-rec set)
    (if (null? set)
      '(())
      (let ((rest (power-set-rec (cdr set))))
        (append rest
                (map (lambda (element) (cons (car set) element))
                     rest)))))
  (cond ((> (length set) 27)
         (error
          '<procedure-POWER-SET>
          "\nA power set of a set with more than 27 elements need to use virtual memory.\n"))
        ((> (length set) 20)
         (display
          (string-append "<procedure-POWER-SET>:\nThe set has cardinality "
                         (number->string (length set))
                         ".\nDanger!!!\n"))))
  (power-set-rec set))

(define (apply-rec op l)
  ;;This procedure is designed particularly for
  ;;summing reciprocals of natural numbers
  (define (length>=2? l)
    (and (pair? l)
         (not (null? (cdr l)))))
  (define (apply-one-iter ll)
    (if (length>=2? ll)
        (append (list (op (car ll)
                          (cadr ll)))
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

(define (highest-power x p)
  ;;This function returns k, where p^k||x
  ;;In this function, p can also be a composite number
  (cond ((= p 1)
         (error
          '<procedure-HIGHEST-POWER>
          (string-append
           "\nThere is no k such that 1^k = "
           (number->string x)
           ".\n")))
        ((> (modulo x p) 0) 0)
        (else (+ 1 (highest-power (/ x p) p)))))

;; (define (factor n primes-list)
;;   (if (null? primes-list)
;;       (error
;;        '<procedure-FACTOR>
;;        (string-append
;;         "\nThe number "
;;         (number->string n)
;;         " contains a prime factor\nthat is not in \"primes-list\".\n"))
;;       (let ((p (car primes-list)))
;;         (cond ((= n 1)
;;                '())
;;               ((divide? p n)
;;                (append (list (list p (highest-power n p)))
;;                        (factor
;;                         (/ n (expt p (highest-power n p)))
;;                         (cdr primes-list))))
;;               (else
;;                (factor n (cdr primes-list)))))))

(define (factor x)
  (define (factor-rec x primes-list)
    (if (null? primes-list)
        (error
         '<procedure-FACTOR>
         (string-append
          "\nThe number "
          (number->string x)
          " contains a prime factor\nthat is not in \"primes-list\".\n"))
        (let ((p (car primes-list)))
          (cond ((= x 1)
                 '())
                ((divide? p x)
                 (append (list (list p (highest-power x p)))
                         (factor-rec
                          (/ x (expt p (highest-power x p)))
                          (cdr primes-list))))
                (else
                 (factor-rec x (cdr primes-list)))))))
  (factor-rec x primes-list))

(define (prime-divisors x)
  (map car (factor x)))

(define (greatest-prime-power factorization)
  (let ((prime-powers
         (map (lambda (x) (expt (car x) (cadr x))) factorization)))
    (apply max prime-powers)))

(define (set-difference s1 s2)
  (cond ((null? s1)
         '())
        ((not (member (car s1) s2))
         (cons (car s1) (set-difference (cdr s1) s2)))
        (else
         (set-difference (cdr s1) s2))))

(define (set-symmetric-difference s1 s2)
  (append (set-difference s1 s2) (set-difference s2 s1)))

(define (set-intersection s1 s2)
  (define (recur s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
          ((= (car s1) (car s2))
           (cons (car s1)
                 (recur (cdr s1) (cdr s2))))
          ((< (car s1) (car s2))
           (recur (cdr s1) s2))
          (else
           (recur s1 (cdr s2)))))
  (recur (sort < s1) (sort < s2)))

(define (sets-intersection sets)
  (if (length>=2? sets)
      (set-intersection (car sets)
                        (sets-intersection (cdr sets)))
      (car sets)))

(define (set-dedupe set)
  (cond ((null? set) '())
        (else
         (if (member (car set) (cdr set))
             (set-dedupe (cdr set))
             (cons (car set) (set-dedupe (cdr set)))))))
