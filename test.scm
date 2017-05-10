(load "efrac.so")


(set! D (range 1 10000))
(set! r (/ 212434 163829))
(factor 163829 primes-list)
(set! DD (rule-out-non-smooth-denominators D 10000 r))
(set! DD D)
(length DD)
(set! sD (reciprocal-sum D))
(set! sDD (reciprocal-sum DD))
(* 1.0 sD)
(* 1.0 sDD)
(set! factorDenomSDD (factor (denominator (- sDD r)) primes-list))
(map
 (lambda (x)
   (cons
    x
    (length
     (filter
      (lambda (y)
        (divide?
         (expt (car x)
               (cadr x))
         y))
      D))))
 factorDenomSDD)
(length (map (lambda (y) (/ y (expt 181 1))) (filter (lambda (x) (divide? (expt 181 1) x)) DD)))


(subset-sum-to-n (list 1 2 3 3 4 5) 12)

(time (length (subset-sum-to-n
               (map (lambda (x)
                      (/ (apply lcm (range 1 29)) x))
                    (range 1 29))
               (* 2 337))))

(sum (map (lambda (y) (modulo y 337)) 
               (map (lambda (x)
                      (/ (apply lcm (range 1 29)) x))
                    (range 1 29))))
(time (length (subset-sum-to-n
               (map (lambda (y) (modulo y 337))
                    (map (lambda (x)
                           (/ (apply lcm (range 1 29)) x))
                         (range 1 29)))
               (* 12 337))))
(subset-sum-to-multiple-n
 (map (lambda (x)
        (/ (apply lcm (range 1 29)) x))
      (range 1 29))
 337)
(define (r n)
  (if (= n 0)
      '()
      (append
       (r (dec1 n))
       (list (length (subset-sum-to-n
                      (map (lambda (x)
                             (/ (apply lcm (range 1 97)) x))
                           (range 1 97))
                      103))))))
(r 100)
(sum (map (lambda (x)
            (/ (apply lcm (range 1 97)) x))
          (range 1 97)))

(define (subset-sum-to-multiple-n l n)
  (define (attach-index l n)
    (if (null? l)
        '()
        (append (list (list (car l) n))
                (attach-index (cdr l) (inc1 n)))))
  (define (subset-sum-to-n SIRL n s) ;SIRL stands for sortedIndexedRL
    (define (append-e e rest)
      (map (lambda (x)
             (append x (list e)))
           rest))
    (define (hf l n s)
      (cond ((= n 0) '(()))
            ((null? l) '())
            (else
             (let ((e (car l)))
               (if (< (- s (car e)) n)
                   (append-e (cdr e)
                             (hf (filter
                                  (lambda (x) (not (> (car x) (- n (car e)))))
                                  (cdr l))
                                 (- n (car e))
                                 (- s (car e))))
                   (append
                    (hf (cdr l) n s)
                    (append-e (cdr e)
                              (hf (filter
                                   (lambda (x) (not (> (car x) (- n (car e)))))
                                   (cdr l))
                                  (- n (car e))
                                  (- s (car e))))))))))
    (hf SIRL n s))
  (define (collect-results SIRL n s)
    (define (iter SIRL n s i iBound)
      (if (> i iBound)
          '()
          (append
           (list (subset-sum-to-n SIRL (* i n) s))
           (iter SIRL n s (inc1 i) iBound))))
    (let ((iBound (floor (/ s n))))
      (iter SIRL n s 1 iBound)))
  (let ((reducedL (map (lambda (x) (modulo x n)) l)))
    (let ((sortedIndexedRL
           (sort (lambda (x y)
                   (< (car x) (car y)))
                 (attach-index reducedL 0)))
          (s (sum reducedL)))
      (collect-results sortedIndexedRL n s))))

(subset-sum-to-multiple-n
 (map (lambda (x)
        (/ (apply lcm (range 1 29)) x))
      (range 1 29))
 337)

(map (lambda (y) (modulo y 337))
     (map (lambda (x)
            (/ (apply lcm (range 1 29)) x))
          (range 1 29)))

(set! power-zeros (power-set (map cdr A)))
(set! rest (list (list 'a 'b) (list 'c 'd)))
(length (apply append
       (map (lambda (x)
              (map (lambda (y) (append x y))
                   power-zeros))
            rest))
)
(cdar A)
(set! A (list (cons 1 2) (cons 3 4) (cons 5 6) (cons 3 8)))
(sort-A A)
(set! B (list (list 1 2) (list 3 4) (list 5 6)))
(member (lambda (x) (> x 3)) D)
(kill-n (list 1 2 3) 3 1)
(set! D (list 3 4 5 6 7))
(set! n 3)
(set! M (list 3 6))
(set! l 2)
(set! B (list 2 1))
  ((
    (let ((l (/ (lcm M) n)))
      (let ((A (map (lambda (m) (cons (modulo (/ (* l n) m) n) m))
                    M)))
        (let ((sr (apply (lambda (a1 a2) (+ a1 (caar a2)))
                         0
                         A))))))))

(define A (list (cons 1 2) (cons 2 3 ) (cons 3 4)))

(apply + (list 1 2 3 4))
(apply + 0 (list 1 2 3 4))

(apply (lambda x (+ x)) (list 1 2 3 4))

(apply (lambda (x y z) (+ x y z)) (list 1 2 3))

(apply + (map car A))

(map (lambda (x y) (+ (car x) y))
     (list (cons 1 2) (cons 2 3) (cons 3 4))
     (list 7 8 9))
;;Test recur
(set! A (list (cons 1 'a) (cons 2 'b) (cons 3 'c) (cons 3 'd) (cons 4 'e) (cons 5 'f)))
(recur A 12 18)

;;Test kill-n
(set! D (range 1 1000))
(set! r (reciprocal-sum D))
(factor (denominator r) primes-list)
(set! sD (reciprocal-sum D))
(* 1.0 sD)
(set! factorDenomSD (factor (denominator r) primes-list))
(map
 (lambda (x)
   (cons
    x
    (length
     (filter
      (lambda (y)
        (divide?
         (expt (car x)
               (cadr x))
         y))
      D))))
 factorDenomSD)
(set! n 3)
(length (kill-n D n r))
(set! M (filter (lambda (x) (divide? n x)) D))
(map (lambda (y) (map (lambda (x) (/ x n)) y)) rest)
(set! l (/ (apply lcm M) n))
(set! A (map (lambda (x) (cons (modulo (/ (* l n) x) n) x)) M))
(set! sum-r (apply + (map car A)))
(set! aim 321)
(length (generate-sets-aim A 0 sum-r))
(untrace generate-sets-aim null?)
(set! zero-pairs (filter (lambda (x) (= (car x) 0)) A))
(set! non-zero-pairs (filter (lambda (x) (> (car x) 0)) A))
(set! power-zeros (power-set (map cdr zero-pairs)))
(set! rest (recur non-zero-pairs aim sum-r))
(set! result (map (lambda (x)
                    (map (lambda (y) (append x y))
                         power-zeros))
                  rest))(car result)
(set! MC (filter (lambda (x) (not (divide? n x))) D))
(recur A 0 1)
(+ 1 1)
(length (kill-n D 5 (reciprocal-sum D)))
(generate-sets-aim A 14 sum-r)
(recur-gen-sets-aim A 2 sum-r)
(reciprocal-sum '())
(set! aim 14)
rest
(set! power-zeros (power-set (list 'a 'b)))
(set! denoms '(7 14 28))
(set! complement (set-difference M denoms))
(set! difference (- r (reciprocal-sum )))

(for-each (lambda (x) (let ((s (reciprocal-sum x)))
                        (display (/ (denominator s) 3))
                        (newline))) (kill-n D 3 r))

(map (lambda (x) (filter (lambda (y) (divide? 3 y)) x)) (kill-n D 3 r))


(- r (/ 1 3))

(set! result (if (null? rest)
                 (map (lambda (x)
                        (map (lambda (y) (append x y))
                             power-zeros))
                      '(()))
                 (map (lambda (x)
                        (map (lambda (y) (append x y))
                             power-zeros))
                      rest)))



(set! D (range 1 1000))
(set! r (reciprocal-sum D))
(factor (denominator r) primes-list)
(length (kill-n D 107 r))
(map (lambda (x) (map (lambda (z) (/ z 107)) (filter (lambda (y) (divide? 107 y)) x))) (kill-n D 107 r))
(time (length (kill-n D 147 r)))

1000 kill 107
1 9 8 4 missed

(set! D (car (kill-n D 3 (- r 3))))
(set! r (reciprocal-sum D))
(* 1.0 r)
(factor (denominator r) primes-list)
(set! D (list 1 2 3 5 6 9 10 15 18 24 20 8 4))
(set! s (list (list (list 1 2 3) (list 1 2 3))))
(map car s)
(apply append s)

(set! s (list (list 1 2) (list 3 4)))
(set! z (list (list 'a) (list 'b)))
(apply append (map (lambda (x) (map (lambda (y) (append x y)) s)) z))