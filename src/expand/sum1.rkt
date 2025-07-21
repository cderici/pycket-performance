;;; SUM1 -- One of the Kernighan and Van Wyk benchmarks.
#lang racket/base
(require "conf.rkt")
(define outer 100) ; 20
;(define sum1-iters 1) ;70)

(define inport #f)

(define (sumport port sum-so-far)
  (let ((x (read port)))
    (if (eof-object? x)
        sum-so-far
        (sumport port (FLOAT+ x sum-so-far)))))

(define (sum port)
  (sumport port 0.0))

(define (go)
  (set! inport (open-input-file "data/rn100"))
  (let ((result (sum inport)))
    (close-input-port inport)
    result))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i sum1-iters) (void))
            (go)))))

(main)
