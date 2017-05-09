(load "math.so")
(load "ext.so")
(load "efrac-branch.so")
;(compile-file "math.scm")
;(compile-file "ext.scm")
;(compile-file "efrac-branch.scm")

;TODO: Implement kill-n branch version

(define (kill-n D rat n r)
  (define (kill-test denoms M)
    (let ((complement (set-difference M denoms)))
       (let ((difference (- r (rec-sum complement))))
         (and (not (divide? n (denominator difference)))
              (not (< difference 0))))))
  (define (k-filter branches rat n)
    (filter (lambda (x)
              (not (divide?
                    n
                    (denominator
                     (- (rec-sum x) rat)))))
            branches))
  (define (append-e e rest)
    (map (lambda (x)
           (merge < x (list e)))
         rest))
  (define sort-A
    (lambda (a) (sort (lambda (x y) (> (car x) (car y))) a)))
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
  (define (generate-sets-aim A aim sum-r)
    (let ((zero-pairs (filter (lambda (x) (= (car x) 0)) A))
           (non-zero-pairs (filter (lambda (x) (> (car x) 0)) A)))
       (let ((rest (recur non-zero-pairs aim sum-r)))
         (cond ((and (= aim 0) (null? rest)) 
                (power-set (map cdr zero-pairs)))
               ((and (> aim 0) (null? rest)) 
                '())
               (else
                (let ((power-zeros (power-set (map cdr zero-pairs))))
                  (apply append
                         (map (lambda (x) (map (lambda (y) (append y x))
                                               power-zeros))
                              rest))))))))
  (define (generate-sets A sum-r)
    (define (recur-gen-sets-aim A i sum-r)
      (if (< i 0)
          '()
          (append (generate-sets-aim A (* i n) sum-r)
                  (recur-gen-sets-aim A (dec1 i) sum-r))))
    (let ((i (floor (/ sum-r n))))
      (recur-gen-sets-aim A i sum-r)))
  (let ((M (filter (lambda (x) (divide? n x)) D))
        (MC (filter (lambda (x) (not (divide? n x))) D)))
    (let ((l (/ (apply lcm M) n)))
      (let ((A (map (lambda (x)
                      (cons (modulo (/ (* l n) x) n)
                            x))
                    M)))
        (let ((sum-r (apply + (map car A))))
          (let ((branch-counter-part
                 (filter (lambda (x) (kill-test x M))
                         (generate-sets A sum-r))))
            (map (lambda (y) (sort < y))
                   (map (lambda (x) (append MC x)) branch-counter-part))))))))

(define (kill-nn D rat n r)
  (define (kk-filter branches bd n)
    (filter (lambda (x)
              (let ((v (+ bd (rec-sum x))))
                 (cond ((< v 0) #f)
                       ((divide? n (denominator v)) #f)
                       (else #t))))
            branches))
  (let ((M (filter (lambda (x) (divide? n x)) D))
        (MC (filter (lambda (x) (not (divide? n x))) D)))
    (let ((bd (- (rec-sum MC) rat))
          (branches (power-set M)))
      (map (lambda (x) (merge < MC x))
           (kk-filter branches bd n)))))

(define (kill-raw D r n)
 (define (test x)
    (let ((difference (- (rec-sum x) r)))
      (cond ((< difference 0) #f)
            ((integer? difference) #t)
            (else (not (divide? n (denominator difference)))))))
  (let ((M (filter (lambda (x) (divide? n x)) D))
        (NM (filter (lambda (x) (not (divide? n x))) D)))
    (let ((power-set-M (power-set M)))
      (filter test (map (lambda (x) (merge < NM x)) power-set-M)))))

(define (kill-ps D r n) ;kill by generating power set
  (define (test x bound)
    (let ((difference (+ (rec-sum x) bound)))
      (cond ((< difference 0) #f)
            ((integer? difference) #t)
            (else (not (divide? n (denominator difference)))))))
  (let ((M (filter (lambda (x) (divide? n x)) D))
        (NM (filter (lambda (x) (not (divide? n x))) D)))
    (let ((power-set-M (power-set M))
          (bound (- (rec-sum NM) r)))
      (map (lambda (x) (merge < NM x))
           (filter (lambda (y) (test y bound))
                   power-set-M)))))

(define (kill-ps-br branch r n)
  (define (test x bound)
    (let ((difference (+ (rec-sum x) bound)))
      (cond ((< difference 0) #f)
            ((integer? difference) #t)
            (else (not (divide? n (denominator difference)))))))
  (let ((M (filter (lambda (x) (divide? n x)) (br-denominators branch)))
        (NM (filter (lambda (x) (not (divide? n x))) (br-denominators branch))))
    (let ((sum-NM (- (br-sum branch) (sum M)))
          (rec-sum-NM (- (br-rec-sum branch) (rec-sum M))))
    (let ((power-set-M (power-set M))
          (bound (- rec-sum-NM r)))
      (map (lambda (x)
             (make-br (+ sum-NM (sum x))
                      (+ rec-sum-NM (rec-sum x))
                      (merge < NM x)))
           (filter (lambda (y) (test y bound))
                   power-set-M))
      ))))
(set! D (range 1 30))
(set! branch (make-br (sum D) (rec-sum D) D))

(define (kill D r n)
  (if (> (gcd (denominator r) n) 1)
      (kill-ps D r n)
      (kill-n D r n (- (rec-sum D) r))))

(define kill-br kill-ps-br)

(define kill kill-raw)

(define (efrac-representation D r)
  (define (rec collector list-of-D)
    (display "----------------------\ncollector    list-of-D\n")
    (display (length collector))
    (display "            ")
    (display (length list-of-D))
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
                                    (- (rec-sum x)
                                       r)))))))
                  (for-each
                   (lambda (y)
                     (let ((difference (- (rec-sum y) r)))
                       (cond ((and (integer? difference)
                                   (not (member y collector)))
                              (set! collector (cons y collector)))
                             ((and (not (integer? difference))
                                   (not (member y new-list-of-D)))
                              (set! new-list-of-D (cons y new-list-of-D))))))
                   new-branches)))
              list-of-D)
             (rec collector new-list-of-D)))))
  (if (integer? (- (rec-sum D) r))
      (list D)
      (rec '() (list D))))







(define (efrac-representation-br D r)
  (define (rec collector branches)
    (display "----------------------\ncollector    branches\n")
    (display (length collector))
    (display "            ")
    (display (length branches))
    (newline)
    (cond ((null? branches) (map br-denominators collector))
          (else
           (let ((new-branches '()))
             (for-each
              (lambda (branch)
                (let ((sub-branches
                       (kill-br branch
                                r
                                (greatest-prime-power
                                 (factor
                                  (denominator
                                   (- (br-rec-sum branch)
                                      r)))))))
                  (for-each
                   (lambda (sub-branch)
                     (let ((difference (- (br-rec-sum sub-branch) r)))
                       (cond ((and (integer? difference)
                                   (not (member sub-branch collector)))
                              (set! collector (cons sub-branch collector)))
                             ((and (not (integer? difference))
                                   (not (member sub-branch new-branches)))
                              (set! new-branches (cons sub-branch new-branches))))))
                   sub-branches))
                )
              branches)
             (rec collector new-branches)))))
  (if (integer? (- (rec-sum D) r))
      (list D)
      (rec '() (list (make-br D)))))

(time (set! s1 (efrac-representation-br (range 1 24) 131/84)))
(time (set! s2 (efrac-representation (range 1 24) 131/84)))
(set-difference s1 s2)
(set-difference s2 s1)

;1, 6, 24, 65, 184,

;; ((set! rat 7/18)
;; (set! n 27)
;; (set! D (range 1 160))
;; (time (set! s1 (kill-raw D rat n)))
;; (time (set! s2 (kill-n D rat n (- (rec-sum D) rat))))
;; (time (set! s3 (kill-nn D rat n (- (rec-sum D) rat))))
;; (time (set! s4 (kill-ps D rat n))))
;; (greatest-prime-power (factor (denominator (rec-sum D))))
;; (car s1)
;; s1
;; s2
;; s3
;; (set-difference (map (lambda (x) (sort < x)) s1) (map (lambda (x) (sort < x)) s2))
;; (set-difference (map (lambda (x) (sort < x)) s1) (map (lambda (x) (sort < x)) s3))
;; (set-difference (map (lambda (x) (sort < x)) s1) (map (lambda (x) (sort < x)) s4))
;; (set-difference s1 s2)
;; (set-difference s1 s3)
;; (set-difference s1 s4)


;; (map rec-sum s2)
;; (map (lambda (x) (divide? 7 (denominator (rec-sum x)))))

;; (display "\nhi\n")

;; (set! M (filter (lambda (x) (divide? 17 x)) (range 1 10000)))
;; (set! NM (filter (lambda (x) (not (divide? 17 x))) (range 1 10000)))
;; (time (set! sorted (sort < (append NM M))))
;; (time (set! sorted (merge < NM M)))
;; (merge < '(1 3 4) '(2 4 6))
;; (time (set! garbage (member (range 1 10000)
;;                             (map (lambda (x) (range 1 x)) (range 1 10010)))))

;; (set! a '(1 2 3 4))
;; (set-cadr! a 5)
;; (define (set-cadr! a b)
;;   (set-car! (cdr a) b))
;; (set-cadr! a 5)
