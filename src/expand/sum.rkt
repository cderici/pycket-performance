#lang racket/base

(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")

      (define (run n)
        (let loop ((i n) (sum 0))
          (if (< i 0)
            sum
            (loop (- i 1) (+ i sum)))))

      ))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (expand stx))))

(main)
