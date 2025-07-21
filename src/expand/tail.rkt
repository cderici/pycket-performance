;;; TAIL -- One of the Kernighan and Van Wyk benchmarks.

#lang racket/base
(require "conf.rkt")
(define outer 100) ; 20
;(define tail-iters 1) ;8)

(define inport #f)
(define outport #f)

(define (readline port line-so-far)
  (let ((x (read-char port)))
    (cond ((eof-object? x)
           x)
          ((char=? x #\newline)
           (list->string (reverse
                          (cons x line-so-far))))
          (#t (readline port (cons x line-so-far))))))

(define (tail-r-aux port file-so-far)
  (let ((x (readline port '())))
    (if (eof-object? x)
        (begin
          (display file-so-far outport)
          (close-output-port outport))
        (tail-r-aux port (cons x file-so-far)))))

(define (tail-r port)
  (tail-r-aux port '()))

(define (go)
  (set! inport (open-input-file "data/bib-tail"))
  (set! outport (open-output-file/truncate "foo-tail"))
  (tail-r inport)
  (close-input-port inport))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i tail-iters) (void))
            (go)))))

(main)
