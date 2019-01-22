;;; CAT -- One of the Kernighan and Van Wyk benchmarks.

#lang racket/base
(require "conf.rkt")
(define outer 10) ; 20
;(define cat-iters 1) ;8)

(define inport #f)
(define outport #f)

(define (catport port)
  (let ((x (read-char port)))
    (if (eof-object? x)
        (close-output-port outport)
        (begin
          (write-char x outport)
          (catport port)))))

(define (go)
  (set! inport (open-input-file "data/bib-cat"))
  (set! outport (open-output-file/truncate "foo-cat"))
  (catport inport)
  (close-input-port inport))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i cat-iters) (void))
            (go)))))

(main)
