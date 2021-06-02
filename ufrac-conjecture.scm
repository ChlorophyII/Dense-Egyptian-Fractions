(define (sld-choose-c d bound)
  (let* ([gpp-list (map (lambda (x)
			  (greatest-prime-power
			   (factor
			    (denominator
			     (+ (/ 1 d)
				(/ 1 (* d x)))))))
			(range 2 bound))]
	 [min-gpp (apply min gpp-list)])
    (inc1 (- bound (length (memq min-gpp gpp-list))))))

(define (sld d c)
  (define (recur TLDs)
    (cond [(null? TLDs) #f]
	  [else
	   (let* ([D (range 1 (car TLDs))]
		  [sol (ufrac-dfs D
				  (- (- 1 (/ 1 d))
				     (/ 1 (* d c))))])
	     (if sol
		 (append sol (list d (* d c)))
		 (recur (cdr TLDs))))]))
  (let ([TLDs (cond [(<= d 100) (list (dec1 d))]
		    [(divide? 20 (dec1 d)) (range 100 (dec1 d) 20)]		    
		    [else (append (range 100 (dec1 d) 20) (list (dec1 d)))])
	      ;; TLD stands for third largest denominator
	      ])
    (recur TLDs)))

(define (sld-witness start end bound)
  ;; prints witnesses for start <= d <= end and c <= bound
  (for-each (lambda (d)
	    (display (sld d (sld-choose-c d bound)))
	    (newline))
	  (range start end)))
