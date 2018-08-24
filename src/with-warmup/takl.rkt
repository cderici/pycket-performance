;;; TAKL -- The TAKeuchi function using lists as counters.
#lang racket/base
#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 100) ;20)
;(define takl-iters 1) ;2000)

(define (listn n)
  (if (= n 0)
    '()
    (cons n (listn (- n 1)))))
 
(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))
 
(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))
 
(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i takl-iters) (void))
            (mas l18 l12 l6)))))

(main)
