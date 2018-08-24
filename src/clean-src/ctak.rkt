;;; CTAK -- A version of the TAK procedure that uses continuations.
#lang racket/base

#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 1) ;20)
(define ctak-iters 1) ;500)

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call-with-current-continuation
       (lambda (k)
         (ctak-aux
          k
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- x 1) y z)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- y 1) z x)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i ctak-iters) (void))
            (ctak 18 12 6)))))

(main)
