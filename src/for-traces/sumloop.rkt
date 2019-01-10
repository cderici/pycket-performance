;;; SUMLOOP -- One of the Kernighan and Van Wyk benchmarks.
#lang racket/base
#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 10) ;20)
;(define sumloop-iters 1) ;40)

(define sum 0)

(define (tail-rec-aux i n)
  (if (< i n)
      (begin (set! sum (+ sum 1)) (tail-rec-aux (+ i 1) n))
      sum))

(define (tail-rec-loop n)
  (set! sum 0)
  (tail-rec-aux 0 n)
  sum)

(define (do-loop n)
  (set! sum 0)
  (do ((i 0 (+ i 1)))
      ((>= i n) sum)
    (set! sum (+ sum 1))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i sumloop-iters) (void))
            (do-loop 100000000)))))

(main)
  
