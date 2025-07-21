;;; FIBFP -- Computes fib(35) using floating point
#lang racket/base


(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")

      (define (fibfp n)
        (if (FLOAT< n 2.)
          n
          (FLOAT+ (fibfp (FLOAT- n 1.))
                  (fibfp (FLOAT- n 2.)))))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (expand stx))))

(main)
