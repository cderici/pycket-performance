;;; STRING -- One of the Kernighan and Van Wyk benchmarks.
#lang racket/base

#;(require "both.rkt" "conf.rkt")
(require "conf.rkt")
(define outer 1) ;20)
(define string-iters 1) ;600)

(define s "abcdef")

(define (grow)
  (set! s (string-append "123" s "456" s "789"))
  (set! s (string-append
           (substring s (quotient (string-length s) 2) (string-length s))
           (substring s 0 (+ 1 (quotient (string-length s) 2)))))
  s)

(define (trial n)
  (do ((i 0 (+ i 1)))
      ((> (string-length s) n) (string-length s))
    (grow)))

(define (my-try n)
  (do ((i 0 (+ i 1)))
      ((>= i 10) (string-length s))
    (set! s "abcdef")
    (trial n)))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i string-iters) (void))
            (my-try 500000)))))

(main)