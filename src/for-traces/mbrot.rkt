;;; MBROT -- Generation of Mandelbrot set fractal.
#lang racket/base

(require "conf.rkt")

(define outer 10) ;20)
;(define mbrot-iters 1) ;3500)

(define (count r i step x y)

  (let ((max-count 64)
        (radius^2  16.0))

    (let ((cr (FLOAT+ r (FLOAT* (exact->inexact x) step)))
          (ci (FLOAT+ i (FLOAT* (exact->inexact y) step))))
      
      (let loop ((zr cr)
                 (zi ci)
                 (c 0))
        (if (= c max-count)
          c
          (let ((zr^2 (FLOAT* zr zr))
                (zi^2 (FLOAT* zi zi)))
            (if (FLOAT> (FLOAT+ zr^2 zi^2) radius^2)
              c
              (let ((new-zr (FLOAT+ (FLOAT- zr^2 zi^2) cr))
                    (new-zi (FLOAT+ (FLOAT* 2.0 (FLOAT* zr zi)) ci)))
                (loop new-zr new-zi (+ c 1))))))))))

(define (mbrot matrix r i step n)
  (let loop1 ((y (- n 1)))
    (when (>= y 0)
      (let loop2 ((x (- n 1)))
        (if (>= x 0)
          (begin
            (vector-set! (vector-ref matrix x) y (count r i step x y))
            (loop2 (- x 1)))
          (loop1 (- y 1)))))))

(define (test n)
  (let ((matrix (make-vector n)))
    (let loop ((i (- n 1)))
      (when (>= i 0)
        (begin
          (vector-set! matrix i (make-vector n))
          (loop (- i 1)))))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i mbrot-iters) (void))
            (test 75)))))

(main)
