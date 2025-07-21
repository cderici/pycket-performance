#lang racket/base

(require racket/syntax "conf.rkt")

(define ack-module-sexp
  #'(module ack racket/base
     (define outer 100)
     (define (ack m n)
       (cond ((= m 0) (+ n 1))
             ((= n 0) (ack (- m 1) 1))
             (else (ack (- m 1) (ack m (- n 1))))))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (expand ack-module-sexp))))

(main)

