;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.
#lang racket/base


(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")

      (define (cpstak x y z)

        (define (tak x y z k)
          (if (not (< y x))
              (k z)
              (tak (- x 1)
                   y
                   z
                   (lambda (v1)
                     (tak (- y 1)
                          z
                          x
                          (lambda (v2)
                            (tak (- z 1)
                                 x
                                 y
                                 (lambda (v3)
                                   (tak v1 v2 v3 k)))))))))

        (tak x y z (lambda (a) a)))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (for ([j (in-range expand-inner)])
            (expand stx)))))

(main)
