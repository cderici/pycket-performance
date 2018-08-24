;;; DIVREC -- Benchmark which divides by 2 using lists of n ()'s.
#lang racket/base

#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 100) ;20)
;(define divrec-iters 1) ; 3000000)

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define *ll* (create-n 200))

(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i divrec-iters) (void))
            (recursive-div2 *ll*)))))

(main)
