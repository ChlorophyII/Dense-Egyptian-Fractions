(load "efrac.scm")
(set! verbose? #t)
(efrac-dfs '(1) 0)
(efrac-dfs '(1 1 1) 0)
(efrac-dfs '(1 1 1) 1)
(efrac-dfs '(1 1) 1/2)
(efrac-dfs '(1 1 1) 1/2)
(efrac-dfs '(2 2) 1/2)
(efrac-dfs '(2 2) 2) ; [(or (negative? r) (negative? diff)) '(())]
(efrac-dfs '(2 2 2) 1/2)
(efrac-dfs '(2 2 2 2) 1/2)
(efrac-dfs '(2 2 2 2) 1)
(efrac-dfs '(2 2 4 4) 1) ; aim-mod
(efrac-dfs '(1 2 3 6) 0)
(efrac-dfs '(1 2 3 6) 1)
(efrac-dfs '(1 2 3 6) 2)
(efrac-dfs '(1 2 3 4 5 6) 1)
(efrac-dfs '(1 2 3 4 5 6) 2)
(efrac-dfss '(1 2 3 4 5 6) 1/3)
(efrac-dfss (range 1 24) 3)
(efrac-dfss (range 1 24) 2)
(efrac-dfss '(7 14 21 28) 1/7)
(time (set! a (efrac (range 1 184) 5)))
(time (set! b (efrac-dfs (cons 1 (range 1 184)) 6)))
(time (set! d (efrac-dfss (range 1 550) 6))) 
(time (set! e (efrac-dfs (cons 1 (range 1 468)) 7)))
(time (set! f (efrac-dfs (append (range 1 24) (range 1 24)) 6)))
