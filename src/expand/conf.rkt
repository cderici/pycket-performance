#lang racket/base

#;(require "both.rkt")

(provide (all-defined-out)
         positive? negative? zero?
         quotient modulo remainder odd? even? min max
         > < >= <= = / - + *)

(define-syntax-rule (time expr1 expr ...)
  (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
    (fprintf (current-error-port)
            "RESULT-cpu: ~a.0\nRESULT-gc: ~a.0\nRESULT-total: ~a.0\n"
            cpu gc user)
    (apply values v)))


(define (fatal-error . args)
  (apply error #f args))

(define (call-with-output-file/truncate filename proc)
 (call-with-output-file filename proc #:mode 'binary #:exists 'truncate))

(define (open-output-file/truncate filename)
  (open-output-file filename #:mode 'binary #:exists 'truncate))

(define expand-outer 1000)

; Gabriel benchmarks
(define boyer-iters       150)
(define browse-iters     2000)
(define cpstak-iters     9000)
(define ctak-iters        500)
(define dderiv-iters  8000000)
(define deriv-iters  13000000)
(define destruc-iters    3500)
(define diviter-iters 9000000)
(define divrec-iters  3000000)
(define puzzle-iters     1000)
(define tak-iters       12000)
(define takl-iters       2000)
(define trav1-iters      1000)
(define trav2-iters       250)
(define triangl-iters     100)

; Kernighan and Van Wyk benchmarks
(define ack-iters          80)
(define array1-iters       16)
(define cat-iters           8)
(define string-iters      600)
(define sum1-iters         70)
(define sumloop-iters      40)
(define tail-iters          8)
(define wc-iters            8)

; C benchmarks
(define fft-iters      100000)
(define fib-iters          25)
(define fibfp-iters        20)
(define mbrot-iters      3500)
(define nucleic-iters     150)
(define pnpoly-iters  2000000)
(define sum-iters      350000)
(define sumrec-iters    50000)
(define sumfp-iters    250000)
; (define tfib-iters         20) threads

; Other benchmarks
(define conform-iters     100)
(define dynamic-iters      40)
(define earley-iters      600)
(define fibc-iters      40000)
(define graphs-iters     1000)
(define lattice-iters       2)
(define matrix-iters      800)
(define maze-iters      10000)
(define mazefun-iters    3000)
(define nqueens-iters    7500)
(define paraffins-iters  2000)
(define peval-iters       300)
(define pi-iters           40)
(define primes-iters   350000)
(define ray-iters         350)
(define scheme-iters    50000)
(define simplex-iters  800000)
(define slatex-iters       20)
(define perm9-iters        50)
(define nboyer-iters      250)
(define sboyer-iters      300)
(define gcbench-iters       5)
;; (define compiler-iters    300)

; New benchmarks
(define parsing-iters    1000)
(define gcold-iters      3500)
(define quicksort-iters   500) ; uses bignums in rand, must fix
;; (define nbody-iters         1) ; nondeterministic (order of evaluation)

; Don't specialize fixnum and flonum arithmetic.

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax FLOATvector?
  (syntax-rules ()
    ((FLOATvector? x) (vector? x))))

(define-syntax FLOATvector
  (syntax-rules ()
    ((FLOATvector x ...) (vector x ...))))

(define-syntax FLOATmake-vector
  (syntax-rules ()
    ((FLOATmake-vector n) (make-vector n 0.0))
    ((FLOATmake-vector n init) (make-vector n init))))

(define-syntax FLOATvector-ref
  (syntax-rules ()
    ((FLOATvector-ref v i) (vector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (vector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (vector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+ x ...) (+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (/ x ...))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (negative? x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (positive? x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (zero? x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (abs x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (sin x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (cos x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (atan x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (sqrt x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (min x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (max x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (round x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

; Generic arithmetic.

(define-syntax GENERIC+
  (syntax-rules ()
    ((GENERIC+ x ...) (+ x ...))))

(define-syntax GENERIC-
  (syntax-rules ()
    ((GENERIC- x ...) (- x ...))))

(define-syntax GENERIC*
  (syntax-rules ()
    ((GENERIC* x ...) (* x ...))))

(define-syntax GENERIC/
  (syntax-rules ()
    ((GENERIC/ x ...) (/ x ...))))

(define-syntax GENERICquotient
  (syntax-rules ()
    ((GENERICquotient x y) (quotient x y))))

(define-syntax GENERICremainder
  (syntax-rules ()
    ((GENERICremainder x y) (remainder x y))))

(define-syntax GENERICmodulo
  (syntax-rules ()
    ((GENERICmodulo x y) (modulo x y))))

(define-syntax GENERIC=
  (syntax-rules ()
    ((GENERIC= x y) (= x y))))

(define-syntax GENERIC<
  (syntax-rules ()
    ((GENERIC< x y) (< x y))))

(define-syntax GENERIC<=
  (syntax-rules ()
    ((GENERIC<= x y) (<= x y))))

(define-syntax GENERIC>
  (syntax-rules ()
    ((GENERIC> x y) (> x y))))

(define-syntax GENERIC>=
  (syntax-rules ()
    ((GENERIC>= x y) (>= x y))))

(define-syntax GENERICexpt
  (syntax-rules ()
    ((GENERICexpt x y) (expt x y))))
