(load "math.so")

;; (define (rule-out-lower-bound x)
;;   (define (approx-lower-bound x)
;;     (let ((a (expt (+ (* 1.73205
;;                          (sqrt (* (expt x 3.0)
;;                                   (- (* 27.0 x)
;;                                      2.0))))
;;                       (* 9.0
;;                          (expt x 2.0)))
;;                    (reciprocal 3.0))))
;;       (exact
;;        (ceiling
;;         (/ (* 0.302853
;;               (+ (expt a 2.0)
;;                  (* 1.81712 x)))
;;            a)))))
;;   (define (search-exact-lower-bound lb)
;;     (let ((k (exact (floor (/ x lb)))))
;;       (let ((d (- (* 2 lb)
;;                   (+ (* k k)
;;                      k))))
;;         (if (< d 0)
;;             (inc1 lb)
;;             (search-exact-lower-bound (dec1 lb))))))
;;   (cond ((> x 8)
;;          (search-exact-lower-bound
;;           (approx-lower-bound x)))
;;         ((> x 5) 4)
;;         (else
;;          (error
;;           '<procedure-RULE-OUT-LOWER-BOUND>
;;           "\nAre you a tester? The number is too small!"))))

;; (define (rule-out-non-smooth-denominators l x r)
;;   (let ((prime-divisors-denom-r (prime-divisors (denominator r))))
;;     (define (iter n l)
;;     (if (> n x)
;;         l
;;         (if (and (prime? n)
;;                  (not (member n prime-divisors-denom-r)))
;;             (iter (inc1 n)
;;                   (filter
;;                    (lambda (x) (not (divide? n x)))
;;                    l))
;;             (iter (inc1 n) l))))
;;     (iter (rule-out-lower-bound x) l)))

;; (define (subset-sum-to-n l n)
;;   (define (append-e e rest)
;;     (map (lambda (x)
;;            (append x (list e)))
;;          rest))
;;   (define (hf l n s)
;;     (cond ((= n 0) '(()))
;;           ((null? l) '())
;;           (else
;;            (let ((e (car l)))
;;              (if (< (- s e) n)
;;                  (append-e e
;;                            (hf (filter
;;                                 (lambda (x) (not (> x (- n e))))
;;                                 (cdr l))
;;                                (- n e)
;;                                (- s e)))
;;                  (append
;;                   (hf (cdr l) n s)
;;                   (append-e e
;;                             (hf (filter
;;                                  (lambda (x) (not (> x (- n e))))
;;                                  (cdr l))
;;                                 (- n e)
;;                                 (- s e)))))))))
;;   (hf (sort > l) n (sum l)))

(define (kill-n D rat n r)
  ;; (define (kill-test denoms n r);No problem
  ;;   ;;Return #t if denoms is a valid branch, otherwise #f
  ;;   (let ((difference (- r (reciprocal-sum denoms))))
  ;;     (and (not (< difference 0))
  ;;          (not (divide?
  ;;                n
  ;;                (denominator difference))))))
  ;; (define (kill-test denoms n r)
  ;;   (cond ((null? denoms) #t)
  ;;         ((divide? n (denominator (reciprocal-sum denoms))))))
  
  (define (kill-test denoms M)
    (let ((complement (set-difference M denoms)))
       (let ((difference (- r (reciprocal-sum complement))))
         (and (not (divide? n (denominator difference)))
              (not (< difference 0))))))
  (define (k-filter branches rat n)
    (filter (lambda (x)
              (not (divide?
                    n
                    (denominator
                     (- (reciprocal-sum x) rat)))))
            branches))
  ;; (define (kill-test denoms M) #t)
  (define (append-e e rest);No problem
    (map (lambda (x)
           (append x (list e)))
         rest))
  (define sort-A
    (lambda (a) (sort (lambda (x y) (> (car x) (car y))) a)));No problem
  (define (recur A aim sum-r)
    ;;The first element of each entry of A is strictly positive
    (cond ((= aim 0) '(()))
          ((or (null? A) (< aim 0) (< sum-r 0))
           '())
          (else
           (let ((e1 (caar A)))
             (cond
              ((< (- sum-r e1) aim)
               (append-e (cdar A) (recur (cdr A) (- aim e1) (- sum-r e1))))
              (else ;(> (- sum-r e1) aim)
               (append (append-e
                        (cdar A)
                        (recur (cdr A) (- aim e1) (- sum-r e1)))
                       (recur (cdr A) aim (- sum-r e1)))))))))
  (define (generate-sets-aim A aim sum-r);No problem
    (let ((zero-pairs (filter (lambda (x) (= (car x) 0)) A))
           (non-zero-pairs (filter (lambda (x) (> (car x) 0)) A)))
       (let ((rest (recur non-zero-pairs aim sum-r)))
         (cond ((and (= aim 0) (null? rest)) ;(display "cond1\n")
                (power-set (map cdr zero-pairs)))
               ((and (> aim 0) (null? rest)) ;(display "cond2\n")
                '())
               (else
                (let ((power-zeros (power-set (map cdr zero-pairs))))
                  ;(display "cond3\n")
                  (apply append
                         (map (lambda (x) (map (lambda (y) (append y x))
                                               power-zeros))
                              rest))))))))
  (define (generate-sets A sum-r);No problem
    (define (recur-gen-sets-aim A i sum-r);No problem
      (if (< i 0)
          '()
          (append (generate-sets-aim A (* i n) sum-r)
                  (recur-gen-sets-aim A (dec1 i) sum-r))))
    (let ((i (floor (/ sum-r n))))
      (recur-gen-sets-aim A i sum-r)))
  (let ((M (filter (lambda (x) (divide? n x)) D))
        (MC (filter (lambda (x) (not (divide? n x))) D)));No problem
    (let ((l (/ (apply lcm M) n)))
      (let ((A (map (lambda (x)
                      (cons (modulo (/ (* l n) x) n)
                            x))
                    M)))
        (let ((sum-r (apply + (map car A))))
          ;; (let ((branch-counter-part (generate-sets A sum-r)))
          ;;   (let ((raw-branches
          ;;          (map (lambda (x) (append MC x)) branch-counter-part)))
          ;;     ;(k-filter raw-branches rat n))))))))
          ;;     raw-branches)))))))
          
          (let ((branch-counter-part
                 (filter (lambda (x) (kill-test x M));(kill-test x n r))
                         (generate-sets A sum-r))))
            ;; (map (lambda (x) (filter (lambda (y) (not (member y x))) D))
            ;;      branch-counter-part)
            (map (lambda (x) (append MC x)) branch-counter-part)))))))
(define (kill-nn D rat n r)
  (define (kk-filter branches bd n)
    (filter (lambda (x)
              (let ((v (+ bd (reciprocal-sum x))))
                 (cond ((< v 0) #f)
                       ((divide? n (denominator v)) #f)
                       (else #t))))
            branches))
  (let ((M (filter (lambda (x) (divide? n x)) D))
        (MC (filter (lambda (x) (not (divide? n x))) D)))
    (let ((bd (- (reciprocal-sum MC) rat))
          (branches (power-set M)))
      (map (lambda (x) (append MC x))
           (kk-filter branches bd n)))))

;; (trace-define (kill D rat n)
;;   (if (divide? n (denominator rat))
;;       (kill-nn D rat n (- (reciprocal-sum D) rat))
;;       (kill-n D rat n (- (reciprocal-sum D) rat))))

(define (kill-raw D r n)
  (define (test x)
    (let ((difference (- (reciprocal-sum x) r)))
      (cond ((< difference 0) #f)
            ((integer? difference) #t)
            (else (not (divide? n (denominator difference)))))))
  (let ((M (filter (lambda (x) (divide? n x)) D))
        (NM (filter (lambda (x) (not (divide? n x))) D)))
    (let ((power-set-M (power-set M)))
      (filter test (map (lambda (x) (append NM x)) power-set-M)))))

(define kill kill-raw)

(define (efrac-representation D r)
  (define (rec collector list-of-D)
    (display "--------------\n")
    (display "Initial collector and list-of-D\n") 
    (display collector)
    (newline)
    (display list-of-D)
    (newline)
    (cond ((null? list-of-D) collector)
          (else
           (let ((new-list-of-D '()))
             (for-each
              (lambda (x)
                (let ((new-branches
                       (kill x r (greatest-prime-power
                                  (factor
                                   (denominator
                                    (- (reciprocal-sum x)
                                       r)))))))
                  (display "Initial new-branches and new-list-of-D\n")
                  (display new-branches)
                  (newline)
                  (display new-list-of-D)
                  (newline)
                  (for-each
                   (lambda (y)
                     (let ((difference (- (reciprocal-sum y) r)))
                       (display "difference\n")
                       (display difference)
                       (newline)
                       (cond ((and (integer? difference)
                                   (not (member y collector)))
                              (set! collector (cons y collector))
                              (display "Case1   Collector\n")
                              (display collector)
                              (display "----\n"))
                             ((and (not (integer? difference))
                                   (not (member y new-list-of-D)))
                              (set! new-list-of-D (cons y new-list-of-D))
                              (display "Case2   new-list-of-D\n")
                              (display new-list-of-D)
                              (display "----\n")))))
                   new-branches)))
              list-of-D)
             (display "Arguments for recursive call: collector and new-list-of-D\n")
             (display collector)
             (newline)
             (display new-list-of-D)
             (newline)
             (rec collector new-list-of-D)))))
  (if (integer? (- (reciprocal-sum D) r))
      (list D)
      (rec '() (list D))))

(set! A (efrac-representation (range 1 50) (/ 3 2)))



(set! B (filter (lambda (x) (= (reciprocal-sum x) (/ 3 2))) (power-set (range 1 15))))

(length (filter (lambda (x) (= (length x) 7)) A))
