(define-record-type br
  (fields
   (immutable sum-denoms)
   (immutable rs-denoms)
   (immutable r)
   (immutable original-r)
   (immutable diff)
   (immutable gpp) ; greatest prime power that divides the denominator of diff ; TODO rename it
   (immutable rs-rsvd) ; reciprocal sum of reserved denominators
   (immutable rsvd) ; reserved denominators
   (immutable denoms)
   (immutable denoms-sol))
  (protocol
   (lambda (new)
     (case-lambda [(sum-NM rs-NM rs-M subset-M NM parent)
		   (let* ([rs-subset-M (rec-sum subset-M)]
			  [new-r (- (br-r parent) rs-subset-M)] 
			  [new-diff (- rs-NM new-r)]
			  [new-rsvd (append subset-M (br-rsvd parent))])
		     (new sum-NM ; sum-denoms
			  rs-NM ; rs-denoms
			  new-r ; r
			  (br-original-r parent) ; original-r
			  new-diff ; diff
			  (cond [(integer? new-diff) #f]
				[else
				 (greatest-prime-power
				  (factor (denominator new-diff)))]) ; gpp
			  (+ rs-subset-M (br-rs-rsvd parent)) ;rs-rsvd
			  new-rsvd ; rsvd
			  NM ; denoms
			  (cond [(zero? new-diff) (sort < (append new-rsvd NM))]
				[(zero? new-r) (sort < new-rsvd)]
				[else #f]) ; denoms-sol
			  ))]
		  [(denoms r)
		   (let* ([rs-denoms (rec-sum denoms)]
			  [diff (- rs-denoms r)])
		     (new (sum denoms) ; sum-denoms
			  rs-denoms ; rs-denoms
			  r ; r
			  r ; original-r
			  diff ; diff
			  (cond [(integer? diff) #f]
				[else
				 (greatest-prime-power
				  (factor (denominator diff)))]) ; gpp
			  0 ; rs-rsvd
			  '() ; rsvd
			  (sort < denoms) ; denoms
			  (cond [(zero? diff) (sort < denoms)]
				[(zero? r) '()]
				[else #f]) ; denoms-sol
			  ))]
		  [(sum-denoms rs-denoms r original-r diff gpp
			       rs-rsvd rsvd denoms denoms-sol)
		   (new sum-denoms rs-denoms r original-r diff gpp
			rs-rsvd rsvd denoms denoms-sol)]
		  ))))

(define (br-equal? br-a br-b)
  (and (equal? (br-sum-denoms br-a)
	       (br-sum-denoms br-b))
       (equal? (br-rs-denoms br-a)
	       (br-rs-denoms br-b)) 
       (equal? (br-r br-a)
	       (br-r br-b))
       (equal? (br-original-r br-a)
	       (br-original-r br-b))
       (equal? (br-rs-rsvd br-a)
	       (br-rs-rsvd br-b))
       (equal? (sort < (br-rsvd br-a)) ; rsvd must be sorted
	       (sort < (br-rsvd br-b)))
       (equal? (sort < (br-denoms br-a)) ; denoms must be sorted
	       (sort < (br-denoms br-b)))
       (equal? ((br-denoms-sol br-a)) ; br-denoms-sol is already sorted
	       (br-denoms-sol br-b))))

(define (br-equal-as-sol? br-a br-b)
  (if (not (br-denoms-sol br-a))
      (error
       '<procedure-BR-EQUAL-AS-SOL?>
       "\nThe first branch is not a solution."
       br-a))
  (if (not (br-denoms-sol br-b))
      (error
       '<procedure-BR-EQUAL-AS-SOL?>
       "\nThe second branch is not a solution."
       br-b))
  (and (equal? (br-original-r br-a)
	       (br-original-r br-b))
       (equal? (br-denoms-sol br-a)
	       (br-denoms-sol br-b))))

(define (br-reserve-first-d br)
  ;; Only to be called when diff is a positive integer and r is nonnegative.
  (let ([diff (br-diff br)])
    ;; diff should be the same as original diff, which is expected 
    ;; to be a positive integer.
    (if (not (and (integer? diff) (positive? diff)))
	(error
	 '<procedure-BR-RESERVE-FIRST-D>
	 (format "\ndiff ~s is not a positive integer." diff)))
    (let* ([d (car (br-denoms br))
	      ;; denoms must be nonempty in this case (diff must be
	      ;; strictly positive), otherwise it is a solution.
	      ]
	   [rec-d (/ 1 d)]
	   [r (- (br-r br) rec-d)])
      (if (negative? r)
	  (error
	   '<procedure-BR-RESERVE-FIRST-D>
	   (format "\nInvalid new branch. New r = ~s is negative."
		   (- r rec-d))))
      (make-br (- (br-sum-denoms br) d) ; sum-denoms
	       (- (br-rs-denoms br) rec-d) ; rs-denoms
	       r ; r
	       (br-original-r br) ; original-r
	       diff ; diff
	       #f ; gpp
	       (+ (br-rs-rsvd br) (/ 1 d)) ; rs-rsvd
	       (append (list d) (br-rsvd br)) ; rsvd
	       (cdr (br-denoms br)) ; denoms
	       (if (zero? r)
		   (sort < (append (list d) (br-rsvd br)))
		   #f) ; denoms-sol
	       ))))

(define (br-discard-first-d br)
  ;; Only to be called when diff is a positive integer.
  (let ([old-diff (br-diff br)])
    (if (not (and (integer? old-diff)
		  (positive? old-diff)))
	(error
	 '<procedure-BR-DISCARD-FIRST-D>
	 (format "\ndiff ~s is not a positive integer." old-diff)))
    (let* ([d (car (br-denoms br))
	      ;; denoms must be nonempty in this case,
	      ;; because diff is strictly positive.
	      ]
	   [rec-d (/ 1 d)]
	   [diff (- old-diff rec-d)])
      (make-br (- (br-sum-denoms br) d) ; sum-denoms
	       (- (br-rs-denoms br) rec-d) ; rs-denoms
	       (br-r br) ; r
	       (br-original-r br) ; original-r
	       diff ; diff
	       (cond [(integer? diff) #f]
		     [else (greatest-prime-power (factor d))]) ; gpp
	       (br-rs-rsvd br)
	       (br-rsvd br)
	       (cdr (br-denoms br))
	       (if (zero? diff)
		   (sort < (append (br-rsvd br) (cdr (br-denoms br))))
		   #f)))))

(define (br-reduce br)
  ;; This function recursively
  ;; discards denominators whose reciprocals are greater than r
  ;; and reserves denominators whose reciprocals are greater than diff.
  ;; In a reduced branch, no denominator will have a reciprocal that is
  ;; greater than r or diff.
  (define (recur r diff denoms rsvd)
    (cond [(or (<= r 0) (<= diff 0))
	   (list r diff denoms rsvd)]
	  [else
	   (let ([denoms<1/r (filter (lambda (x) (< x (/ 1 r))) denoms)]
		 [denoms<1/diff (filter (lambda (x) (< x (/ 1 diff))) denoms)])
	     (cond [(and (null? denoms<1/r) (null? denoms<1/diff))
		    (list r diff denoms rsvd)]
		   [(null? denoms<1/diff) ; denoms<1/r is not null and we need to discard them
		    (recur r
			   (- diff (rec-sum denoms<1/r))
			   (remp (lambda (x) (< x (/ 1 r))) denoms)
			   rsvd)]
		   [else ; denoms<1/diff is not null and we need to reserve them
		    (recur (- r (rec-sum denoms<1/diff))
			   diff
			   (remp (lambda (x) (< x (/ 1 diff))) denoms)
			   (append rsvd denoms<1/diff))]))]))
  (let* ([result (recur (br-r br)
			(br-diff br)
			(br-denoms br)
			(br-rsvd br))]
	 [new-r (car result)]
	 [new-diff (cadr result)]
	 [new-denoms (caddr result)]
	 [new-rsvd (cadddr result)])
    (make-br (sum new-denoms)
	     (rec-sum new-denoms)
	     new-r
	     (br-original-r br)
	     new-diff
	     (cond [(integer? new-diff) #f]
		   [else
		    (greatest-prime-power
		     (factor (denominator new-diff)))]) ;gpp
	     (rec-sum new-rsvd)
	     new-rsvd
	     new-denoms
	     (cond [(zero? new-diff)
		    (sort < (append new-rsvd new-denoms))]
		   [(zero? new-r) (sort < new-rsvd)]
		   [else #f])
	     )))

(define (br-display br)
  (printf "                                       original r: ~s\n"
	  (br-original-r br))
  (printf "                                                r: ~s\n"
	  (br-r br))
  (printf "                              sum of denominators: ~s\n"
	  (br-sum-denoms br))
  (printf "                   reciprocal sum of denominators: ~s\n"
	  (br-rs-denoms br))
  (printf "  difference (reciprocal sum of denominators - r): ~s or ~10f\n"
	  (br-diff br) (br-diff br))
  (printf "       factorization of denominator of difference: ~s\n"
	  (factor (denominator (br-diff br))))
  (printf "greatest prime power in denominator of difference: ~s\n"
	  (br-gpp br))
  (printf "                            reserved denominators: ~s\n"
	  (br-rsvd br))
  (printf "                           remaining denominators: ~s\n"
	  (br-denoms br))
  (printf "                                    solution to r: ~s\n"
	  (br-denoms-sol br)))
