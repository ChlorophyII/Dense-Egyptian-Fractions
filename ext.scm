(define (length>=2? l)
  (and (pair? l)
       (not (null? (cdr l)))))

(define (set-cadr! l new-value)
  (if (length>=2? l)
      (set-car! (cdr l) new-value)
      (error
       '<procedure-SET-CADR!>
       "\nFirst argument is not a list with at least two elements")))



