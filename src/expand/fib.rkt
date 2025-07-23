;;; FIB -- A classic benchmark, computes fib(35) inefficiently.
#lang racket/base


(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")

      (define (fib n)
        (if (< n 2)
          n
          (+ (fib (- n 1))
             (fib (- n 2)))))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (for ([j (in-range expand-inner)])
            (expand stx)))))

(main)
