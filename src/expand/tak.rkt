;;; TAK -- A vanilla version of the TAKeuchi function.
#lang racket/base

(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")

      (define (tak x y z)
        (if (not (< y x))
            z
            (tak (tak (- x 1) y z)
                 (tak (- y 1) z x)
                 (tak (- z 1) x y))))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (for ([j (in-range expand-inner)])
            (expand stx)))))

(main)
