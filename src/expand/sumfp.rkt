;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point
#lang racket/base

(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")

      (define (run n)
        (let loop ((i n) (sum 0.))
          (if (FLOAT< i 0.)
            sum
            (loop (FLOAT- i 1.) (FLOAT+ i sum)))))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (expand stx))))

(main)
