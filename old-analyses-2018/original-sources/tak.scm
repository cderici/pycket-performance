;;; TAK -- A vanilla version of the TAKeuchi function.
 
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))
 
(define (main . args)
  (for ([i (build-list 100 (λ (x) x))])
       (run-benchmark
	"tak"
	tak-iters
	(lambda (result) (equal? result 7))
	(lambda (x y z) (lambda () (tak x y z)))
	18
	12
	6)))
