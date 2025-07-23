;;; PRIMES -- Compute primes less than 100, written by Eric Mohr.
#lang racket/base

(require racket/syntax "conf.rkt")

(define stx
  #'(module mod racket/base
      (require "conf.rkt")

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
        (sieve (interval-list 2 n)))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (for ([j (in-range expand-inner)])
            (expand stx)))))

(main)
