;;; ACK -- One of the Kernighan and Van Wyk benchmarks.
#lang racket/base
(require "conf.rkt")
(define outer 10) ; 20
(define ack-iters 1) ; 80

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i ack-iters) (void))
            (ack 3 9)))))

(main)
