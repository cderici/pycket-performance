;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.
#lang racket/base

#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 100) ; 20)
;(define cpstak-iters 1) ;9000) 

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

  (tak x y z (lambda (a) a)))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i cpstak-iters) (void))
            (cpstak 18 12 6)))))

(main)