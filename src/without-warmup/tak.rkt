;;; TAK -- A vanilla version of the TAKeuchi function.
#lang racket/base
#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 1) ;20)
;(define tak-iters 1) ;12000)

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i tak-iters) (void))
            (tak 18 12 6)))))

(main)
