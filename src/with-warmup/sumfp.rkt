;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point
#lang racket/base
(require "conf.rkt")

(define outer 100) ;20)
;(define sumfp-iters 1) ;250000)

(define (run n)
  (let loop ((i n) (sum 0.))
    (if (FLOAT< i 0.)
      sum
      (loop (FLOAT- i 1.) (FLOAT+ i sum)))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i sumfp-iters) (void))
            (run 10000.)))))

(main)
