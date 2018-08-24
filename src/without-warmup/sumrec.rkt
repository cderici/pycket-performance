;;; SUMREC -- Compute sum of integers from 0 to 10000
#lang racket/base

#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 1) ;20)
;(define sum-iters 1) ;350000)

(define (run n)
  (if (< n 0)
      0
      (+ n (run (- n 1)))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i sum-iters) (void))
            (run 10000)))))

(main)
