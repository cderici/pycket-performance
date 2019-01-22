;;; WC -- One of the Kernighan and Van Wyk benchmarks.

#lang racket/base
(require "conf.rkt")
(define outer 10) ; 20
;(define wc-iters 1) ;8)

(define inport #f)

(define nl #f)
(define nw #f)
(define nc #f)
(define inword #f)

(define (wcport port)
  (let ((x (read-char port)))
    (if (eof-object? x)
        (begin
          (list nl nw nc))
        (begin
          (set! nc (+ nc 1))
          (when (char=? x #\newline)
              (set! nl (+ nl 1)))
          (if (or (char=? x #\space)
                  (char=? x #\newline))
              (set! inword #f)
              (when (not inword)
                  (begin
                    (set! nw (+ nw 1))
                    (set! inword #t))))
          (wcport port)))))

(define (go)
  (set! inport (open-input-file "data/bib-wc"))
  (set! nl 0)
  (set! nw 0)
  (set! nc 0)
  (set! inword #f)
  (let ((result (wcport inport)))
    (close-input-port inport)
    result))
 
(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i wc-iters) (void))
            (go)))))

(main)
