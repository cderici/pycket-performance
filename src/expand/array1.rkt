;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.
#lang racket/base
(require racket/syntax "conf.rkt")

(define array1-module-stx
  #'(module array1 racket/base
      (define outer 100)

      (define (create-x n)
        (define result (make-vector n))
        (do ((i 0 (add1 i)))
            ((>= i n) result)
          (vector-set! result i i)))

      (define (create-y x)
        (let* ((n (vector-length x))
               (result (make-vector n)))
          (do ((i (sub1 n) (sub1 i)))
              ((< i 0) result)
            (vector-set! result i (vector-ref x i)))))

      (define (my-try n)
        (vector-length (create-y (create-x n))))

      (define (go n)
        (let loop ((repeat 100)
                   (result '()))
          (if (> repeat 0)
              (loop (sub1 repeat) (my-try n))
              result)))))

(define (main)
  (for ([i (in-range expand-outer)])
    (time (expand array1-module-stx))))

(main)

