;;; SUM -- Compute sum of integers from 0 to 10000

(define (run n k)
  (if (< n 0)
      k
      (run (- n 1) (+ n k))))
 
(define (main . args)
  (for ([i (build-list 100 (λ (x) x))])
       (run-benchmark
	"sum"
	sum-iters
	(lambda (result) (equal? result 50005000))
	(lambda (n) (lambda () (run n 0)))
	10000)))
