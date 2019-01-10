;;; PRIMES -- Compute primes less than 100, written by Eric Mohr.
#lang racket/base
(require "conf.rkt")
(define outer 10) ; 20
;(define primes-iters  1) ; 350000)

(define  (interval-list m n)
  (if (> m n)
    '()
    (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ((remove-multiples
            (lambda (n l)
              (if (null? l)
                '()
                (if (= (modulo (car l) n) 0)
                  (remove-multiples n (cdr l))
                  (cons (car l)
                        (remove-multiples n (cdr l))))))))
    (if (null? l)
      '()
      (cons (car l)
            (sieve (remove-multiples (car l) (cdr l)))))))

(define (primes<= n)
  (sieve (interval-list 2 n)))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i primes-iters) (void))
            (primes<= 100)))))

(main)
