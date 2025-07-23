;;; CTAK -- A version of the TAK procedure that uses continuations.
#lang racket/base

(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")


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
                 (lambda (k) (ctak-aux k (- z 1) x y))))))))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (for ([j (in-range expand-inner)])
            (expand stx)))))


(main)
