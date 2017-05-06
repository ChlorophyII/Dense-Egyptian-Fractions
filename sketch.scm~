(load "efrac.so")
(compile-file "math.scm")
(compile-file "efrac.scm")

(define (egyptian-fraction-representation r x)
  (set! d (range 1 x))
  (set! d (rule-out-non-smooth-denominators d x r))
  (set! sum (reciprocal-sum d))
  (set! denom (denominator sum))
  (set! denom-factor (factor denom primes-list))
  (set! greatest-prime-power (find-greatest-prime-power denom-factor))
  (set! multiples-gpp (filter (lambda (x) (divide? greatest-prime-power x)) d))
  (set! power-set-m-gpp (power-set multiples-gpp))
  (filter (lambda (x)
            (and
             (pair? x)
             (not
              (=
               denom
               (denominator
                (-
                 sum
                 (reciprocal-sum x)))))))
          power-set-m-gpp))

(time (egyptian-fraction-representation (/ 7 19) 20000))

(define a (range 1 500))
(set! a (rule-out-non-smooth-denominators a 500 1))
(define b (map reciprocal a))
(set! s (reciprocal-sum a))
(find-greatest-prime-power (factor (denominator s) primes-list))

(length a)
(length (remv 223 a))
(factor 361 primes-list)
(set! a (remove 223 a))
(map (lambda (x)
       (cons
        (length
         (filter
          (lambda (y)
            (divide? (expt (car x) (cadr x)) y))
          a))
        (car x)))
     (factor (denominator s) primes-list))

(define s (range 5))
(remove 3 s)

(factor (denominator s) primes-list)

(/ (denominator s) 8)

(filter (lambda (x) (divide? 19 x)) a)



(length (kill-n (range 1 24) 2 (reciprocal-sum (range 1 24))))

(map (lambda (x) (/ x 5)) (map sum (kill-n (range 1 25) 5 (reciprocal-sum (range 1 25)))))
(list-ref (kill-n (range 1 25) 5 (reciprocal-sum (range 1 25))) )
(kill-n D n (reciprocal-sum D))
(kill-n D n r)
(set! D (list 7 14 21 28))
(set! D (range 1 30))
(set! n 7)
(set! r (- (reciprocal-sum D) (/ 1 7)))
(denominator r)
(set! M (filter (lambda (x) (divide? n x)) D))
(set! MC (filter (lambda (x) (not (divide? n x))) D))
(set! l (/ (apply lcm M) n))
(set! A (map (lambda (x)
                      (cons (modulo (/ (* l n) x) n)
                            x))
                    M))A
(set! sum-r (apply + (map car A)))sum-r
(set! branch-counter-part
      (filter (lambda (x) (kill-test x M));(kill-test x n r))
              (generate-sets A sum-r)))

(set! i (floor (/ sum-r n)))
(generate-sets-aim A 14 sum-r)
(set! zero-pairs (filter (lambda (x) (= (car x) 0)) A))
(set! non-zero-pairs (filter (lambda (x) (> (car x) 0)) A))

(set! rest (recur non-zero-pairs 0 sum-r))
(kill-test '(28 21) M)

(define (largest-prime-power-factor n)
  (define (recur l)
    (if (null? l)
        '(0)
        (let ((rest (recur (cdr l))))
          (if (> (caar l) (car rest))
            (car l)
            rest))))
  (car (recur (map (lambda (x) (cons (expt (car x) (cadr x)) x))
                   (factor n primes-list)))))

(factor 1123 primes-list)
(trace-define (efrac-helper rat D r)
  (cond ((= r 0) D)
        ((< r 0) '())
        (else (let ((lpp (largest-prime-power-factor (denominator (reciprocal-sum D)))))
                (let ((branches (kill-n D lpp r)))
                  (map (lambda (x) (efrac-helper rat x (- (reciprocal-sum x) rat)))
                       branches))))))

(trace-define (efrac-helper rat D r)
  (if (= r 0)
      D
      (let ((lpp (largest-prime-power-factor (denominator (reciprocal-sum D)))))
        (let ((branches (kill-n D lpp r)))
          (map (lambda (x) (efrac-helper rat
                                         x
                                         (- (reciprocal-sum x) rat)))
               branches)))))


(define (efrac-representation rat x)
  (let ((D (range 1 x)))
    (let ((s (reciprocal-sum D)))
      (efrac-helper rat D (- s rat)))))
(efrac-representation (/ 1 7) 30)

(set! rat 3)
(set! D (range 1 24))
(set! r (- (reciprocal-sum (range 1 24)) rat))
(set! lpp (largest-prime-power-factor (denominator (reciprocal-sum D))))
(set! branches (kill-n D lpp r))branches

(set! D '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24))
(- (reciprocal-sum D) (/ 8 3))
(factor 48 primes-list)
(kill-n D 16 (/ 23 48))

(efrac-helper 1/3 '(1 2 3 5 6 9 10 15 18 24 20 8 4) 8/3)
(reciprocal-sum '(1 2 3 4 5 6 7 8 9 10 12 14 15 18 20 21 24 28 30))
(kill-n '(1 2 3 4 5 6 7 8 9 10 12 14 15 18 20 21 24 28 30) 7 (/ 229 70))

(- (/ 229 70) (/ 1 7))

(kill-n '(7 14 21 28) (/ 1 7) 7 (reciprocal-sum '(14 21 28)))
(kill-n (range 1 30) 5 (reciprocal-sum (range 1 30)))
(kill-n (range 1 30) 7 (- (reciprocal-sum (range 1 30)) (/ 1 7)))

(kill-nn '(7 14 21 28) (/ 1 7) 7 (reciprocal-sum '(14 21 28)))
(set! D '(7 14 21 28))
(set! rat (/ 1 7))
(set! n 7)
(set! r (reciprocal-sum '(14 21 28)))
(set! M (filter (lambda (x) (divide? n x)) D))
(set! MC (filter (lambda (x) (not (divide? n x))) D))
(set! bd (- (reciprocal-sum MC) rat))
(set! branches (power-set M))
(kk-filter branches bd n)
((lambda (x)
              ((let ((v (+ bd (reciprocal-sum x))))
                 (and (or (> v 0) (= v 0))
                      (not (divide? n (denominator v))))))) '(28))

(kill (range 1 30) (/ 1 7) 7)
(kill (range 1 30) 0 5)
(map (lambda (x) (denominator (reciprocal-sum x))) (kill (range 1 24) 0 5))

(map (lambda (y) (filter (lambda (x) (divide? 7 x)) y)) (kill (range 1 24) (/ 1 7) 7))
(map (lambda (y) (filter (lambda (x) (divide? 9 x)) y)) (kill (range 1 24) 0 9))
(map (lambda (y) (filter (lambda (x) (divide? 3 x)) y)) (kill (range 1 24) 0 3))
(kill-nn (range 1 24) 0 9 (reciprocal-sum (range 1 24)))
