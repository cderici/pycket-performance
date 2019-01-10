;;; SUMRECFP -- Compute sum of integers from 0 to 10000

(define (run n)
  (if (FLOAT< n 0.)
      0
      (FLOAT+ n (run (FLOAT- n 1.)))))
 
(define (main . args)
  (for ([i (build-list 100 (λ (x) x))])
       (run-benchmark
	"sum"
	sum-iters
	(lambda (result) (equal? result 50005000.))
	(lambda (n) (lambda () (run n)))
	10000.)))
