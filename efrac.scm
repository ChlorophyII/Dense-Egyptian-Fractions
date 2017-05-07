(load "math.so")

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
    (display "------------------\ncollector    list-of-D\n")
    (display (length collector))
    (display "          ")
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
                                    (- (reciprocal-sum x)
                                       r)))))))
                  (for-each
                   (lambda (y)
                     (let ((difference (- (reciprocal-sum y) r)))
                       (cond ((and (integer? difference)
                                   (not (member y collector)))
                              (set! collector (cons y collector)))
                             ((and (not (integer? difference))
                                   (not (member y new-list-of-D)))
                              (set! new-list-of-D (cons y new-list-of-D))))))
                   new-branches)))
              list-of-D)
             (rec collector new-list-of-D)))))
  (if (integer? (- (reciprocal-sum D) r))
      (list D)
      (rec '() (list D))))


;1, 6, 24, 65, 184,
