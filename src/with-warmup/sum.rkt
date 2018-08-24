#lang racket/base

#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 100) ;20)
;(define sum-iters 1) ;     350000)

(define (run n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
      sum
      (loop (- i 1) (+ i sum)))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i sum-iters) (void))
            (run 10000)))))

(main)
