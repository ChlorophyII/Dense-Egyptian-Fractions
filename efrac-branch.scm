(define (br-map proc branches)
  (map (lambda (x)
         (cons (car x)
               (cons (cadr x)
                     (proc (cddr x)))))
       branches))

(define make-br
  (case-lambda
   ((D) (cons (sum D)
              (cons (rec-sum D)
                    D)))
   ((sum-br rec-sum-br denominators-br)
    (cons sum-br
          (cons rec-sum-br
                denominators-br)))))

(define br-sum car)
(define br-rec-sum cadr)
(define br-denominators cddr)
(define set-sum! set-car!)
(define set-rec-sum! set-cadr!)
