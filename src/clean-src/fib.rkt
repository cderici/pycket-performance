;;; FIB -- A classic benchmark, computes fib(35) inefficiently.
#lang racket/base

#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 1) ;20)
(define fib-iters 1) ;25)

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i fib-iters) (void))
            (fib 35)))))

(main)
