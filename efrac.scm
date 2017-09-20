(load "math.so")
(load "ext.so")
(load "efrac-branch.so")

                                        ;(compile-file "math.scm")
                                        ;(compile-file "ext.scm")
                                        ;(compile-file "efrac-branch.scm")
                                        ;(compile-file "efrac.scm")
                                        ;(load "efrac.so")

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
    (let ((lcm-M (apply lcm M)))
      (let ((A (map (lambda (x)
                      (cons (modulo (/ lcm-M x) n)
                            x))
                    M)))
        (let ((sum-r (apply + (map car A))))
          (let ((branch-counter-part
                 (filter (lambda (x) (kill-test x M))
                         (generate-sets A sum-r))))
            (map (lambda (y) (sort < y))
                 (map (lambda (x) (append MC x)) branch-counter-part))))))))

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

(define (kill D r n)
  (if (> (gcd (denominator r) n) 1)
      (kill-ps D r n)
      (kill-n D r n (- (rec-sum D) r))))

;(define kill kill-raw)

(define (efrac-representation D r)
  (define (rec collector list-of-D)
    (cond ((not (= (length list-of-D) 1))
           (display "----------------------\ncollector    list-of-D\n")
           (display (length collector))
           (display "            ")
           (display (length list-of-D))
           (newline)))
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

(define (kill-n-br branch r n)
  (define (kill-test raw-branch-counter-part-x rec-sum-MC)
    (let ((difference (- (+ rec-sum-MC (car raw-branch-counter-part-x))
                         r)))
      (and (not (divide? n (denominator difference)))
           (not (< difference 0)))))
  (define (append-e e rest)
    (map (lambda (x)
           (merge < x (list e)))
         rest))
  (define (recur N aim sum-r);N = non-zero-pairs
    (cond ((= aim 0) '(()))
          ((or (null? N) (< aim 0) (< sum-r 0))
           '())
          (else
           (let ((e1 (caar N)))
             (cond
              ((< (- sum-r e1) aim)
               (append-e (cdar N) (recur (cdr N) (- aim e1) (- sum-r e1))))
              (else ;(> (sum-r e1) aim)
               (append (append-e
                        (cdar N)
                        (recur (cdr N) (- aim e1) (- sum-r e1)))
                       (recur (cdr N) aim (- sum-r e1))))))))) 
  (define (generate-sets-aim power-zero-pairs non-zero-pairs aim sum-r)
    (let ((rest (recur non-zero-pairs aim sum-r)))
      (cond ((and (= aim 0)); (null? rest))
             power-zero-pairs)
            ((and (> aim 0) (null? rest))
             '())
            (else
             (apply append
                    (map (lambda (x) (map (lambda (y) (merge < x y))
                                          power-zero-pairs))
                         rest))))))
  (define (generate-sets A sum-r p)
    (define (recur-gen-sets-aim power-zero-pairs non-zero-pairs i sum-r p)
      (if (< i 0)
          '()
          (append (generate-sets-aim power-zero-pairs
                                     non-zero-pairs
                                     (* i p)
                                     sum-r)
                  (recur-gen-sets-aim power-zero-pairs non-zero-pairs (dec1 i) sum-r p))))
    (let ((i (floor (/ sum-r p)))
          (power-zero-pairs
           (power-set
            (map cdr
                 (filter (lambda (x) (= (car x) 0))
                         A))))
          (non-zero-pairs (filter (lambda (x) (> (car x) 0)) A)))
      (recur-gen-sets-aim power-zero-pairs non-zero-pairs i sum-r p)))
  (let ((M (filter (lambda (x) (divide? n x)) (br-denominators branch)))
        (MC (filter (lambda (x) (not (divide? n x)))
                    (br-denominators branch)))
        (p (caar (factor n))))
    (let ((l (apply lcm M))
          (sum-MC (- (br-sum branch) (sum M)))
          (rec-sum-MC (- (br-rec-sum branch) (rec-sum M))))
      (let ((A (map (lambda (x)
                      (cons (modulo (/ l x) p)
                            x))
                    M)))
        (let ((sum-r (sum (map car A))))
          (let ((raw-branch-counter-part
                 (map (lambda (x) (cons (rec-sum x) x))
                      (generate-sets A sum-r p))))
            (let ((branch-counter-part
                   (filter (lambda (x) (kill-test x rec-sum-MC))
                           raw-branch-counter-part)))
              (map (lambda (x) (make-br (+ sum-MC (sum (cdr x)))
                                        (+ rec-sum-MC (car x))
                                        (merge < MC (cdr x))))
                   branch-counter-part))))))))

(define (kill-br branch r n)
  (if (> (gcd (denominator r) n) 1)
      (kill-ps-br branch r n)
      (kill-n-br branch r n)))

;(define kill-br kill-n-br)
;(define kill-br kill-ps-br)

(define (efrac-representation-br D r)
  (define (rec collector branches)
    (cond ((not (= (length branches) 1))
           (display "----------------------\ncollector    branches\n")
           (display (length collector))
           (display "            ")
           (display (length branches))
           (newline)))
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
                   sub-branches)))
              branches)
             (rec collector new-branches)))))
  (if (integer? (- (rec-sum D) r))
      (list D)
      (rec '() (list (make-br D)))))

(define efrac efrac-representation-br)
                                        ;1, 6, 24, 65, 184,

(define (efrac-dfs-br D r)
  (let ((collector '())
        (branches (list (make-br D)))
        (counter 1))
    (define (iter)
      (if (= (modulo counter 10) 0)
          (display (string-append
                    "counter: "
                    (number->string counter)
                    "        branches: "
                    (number->string (length branches))
                    "\n")))
      (set! counter (+ counter 1))
      (if (null? collector)
          (if (null? branches)
              '()
              (let ((branch (car branches)))
                (set! branches (cdr branches))
                (let ((new-branches
                       (kill-br branch
                                r
                                (greatest-prime-power
                                 (factor
                                  (denominator
                                   (- (br-rec-sum branch)
                                      r)))))))
                  (for-each
                   (lambda (new-branch)
                     (let ((difference (- (br-rec-sum new-branch) r)))
                       (cond ((and (integer? difference)
                                   (not (member new-branch collector)))
                              (set! collector (cons new-branch collector)))
                             ((and (not (integer? difference))
                                   ;#t)
                                        (not (member new-branch branches)))
                              (set! branches (cons new-branch branches))))))
                   new-branches)
                  (iter))))
          (br-denominators (car collector))))
    (iter)))

(define (efrac-least-rec-sum-br D r)
  ;; The difference between this procedure and efrac-dfs-br is that it
  ;; always operates on the branch with lowest reciprocal sum
  (let ((collector '())
        (branches (list (make-br D)))
        (counter 1))
    (define (iter)
      (if (= (modulo counter 10) 0)
          (display (string-append
                    "counter: "
                    (number->string counter)
                    "        branches: "
                    (number->string (length branches))
                    "\n")))
      (set! counter (+ counter 1))
      (if (null? collector)
          (if (null? branches)
              '()
              (let ((branch (car branches)))
                (set! branches (cdr branches))
                (let ((new-branches
                       (kill-br branch
                                r
                                (greatest-prime-power
                                 (factor
                                  (denominator
                                   (- (br-rec-sum branch)
                                      r)))))))
                  (for-each
                   (lambda (new-branch)
                     (let ((difference (- (br-rec-sum new-branch) r)))
                       (cond ((and (integer? difference)
                                   (not (member new-branch collector)))
                              (set! collector (cons new-branch collector)))
                             ((and (not (integer? difference))
                                        ;#t)
                                   (not (member new-branch branches)))
                              (set! branches
                                    (merge
                                     (lambda (br1 br2)
                                       (< (br-rec-sum br1)
                                          (br-rec-sum br2)))
                                     (list new-branch)
                                     branches))))))
                   new-branches)
                  (iter))))
          (br-denominators (car collector))))
    (iter)))
