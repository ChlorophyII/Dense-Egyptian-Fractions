(set! efrac-5 (efrac-representation-br (range 1 184) 5))
(set! efrac-5-inter (sets-intersection efrac-5))
(* 1.0 (rec-sum efrac-5-inter))
(factor 469)
(set! efrac-6
      (efrac-representation-br
       (set-difference (range 1 467)
                       (merge < '(468) efrac-5-inter))
       (- 6 (+ 1/469 (rec-sum efrac-5-inter)))))
(efrac-representation-br (range 1 468) 6)

