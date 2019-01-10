;;; FIBFP -- Computes fib(35) using floating point
#lang racket/base

#;(require "both.rkt" "conf.rkt")

(require "conf.rkt")
(define outer 10) ;20)
;(define fibfp-iters 1) ;20)

(define (fibfp n)
  (if (FLOAT< n 2.)
    n
    (FLOAT+ (fibfp (FLOAT- n 1.))
            (fibfp (FLOAT- n 2.)))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i fibfp-iters) (void))
            (fibfp 35.)))))

(main)
