#lang racket/base

(require "private/fasl-generator.rkt"
         "private/branchy-generator.rkt"
         racket/cmdline)


(module+ main

  ;; FIXME: refactor this entire program, could be written much better
  ;; to accommodate later customizations

  (define what? #f)
  (define what-kind? #f) ; for fasl
  (define input-size 0)
  (define OUTER_ITER_DEFAULT 10)
  (define INNER_ITER_DEFAULT 2000)
  #;(define wk? 'random)

  (define outer-iteration OUTER_ITER_DEFAULT)
  (define inner-iteration INNER_ITER_DEFAULT)

  (define usage (format
"Usage: racket main.rkt <option> [--outer:15] [--inner:2000]

        <option> : --make-same <number> <how-many?>
                   --make-ones <how-many?>
                   --make-zeros <how-many?>
                   --make-random <how-many?>
                   --make-fasl <kind> <how-many?>

        <kind> : random | integer | flonum | rational | complex | symbol | string | char | boolean | path | regexp | box \n"))

  (command-line
   #:once-any
   ["--make-same" -what? how-many? "" (set! what? -what?) (set! input-size how-many?)]
   ["--make-ones" how-many? "" (set! what? 1) (set! input-size how-many?)]
   ["--make-zeros" how-many? "" (set! what? 0) (set! input-size how-many?)]
   ["--make-random" how-many? "" (set! what? 'random) (set! input-size how-many?)]
   ["--make-fasl" wk? how-many? "" (set! what? 'fasl) (set! what-kind? (and (not (equal? wk? "random")) wk?)) (set! input-size how-many?)]
   #:once-each
   ["--outer" outer-count "" (set! outer-iteration outer-count)]
   ["--inner" inner-count "" (set! inner-iteration inner-count)]
   #:args ()

   ;; Handling/Type correcting the inputs
   (when (string? what?)
     ;; if it's a string than we got a number from cmd line
     (set! what? (string->number what?)))
   (when (string? what-kind?)
     (set! what-kind? (string->symbol what-kind?)))
   (unless (number? input-size)
     (set! input-size (string->number input-size)))
   (unless (number? outer-iteration)
     (set! outer-iteration (string->number outer-iteration)))
   (unless (number? inner-iteration)
     (set! inner-iteration (string->number inner-iteration)))

   ;; mkdir generated if it doesn't exist
   (define (ensure-gen-dir)
     (let ([target-path (build-path (current-directory) GEN_DIR)])
       (unless (directory-exists? target-path)
         (make-directory target-path))))

   (cond
     [(number? what?)
      (begin
        (printf "Generating : all-~as-~a-~a-~a.rkt\n"
                what? input-size outer-iteration inner-iteration)
        (ensure-gen-dir)
        (make-all-same what? input-size outer-iteration inner-iteration))]
     [(equal? what? 'random)
      (begin
        (printf "Generating : random38-~a-~a-~a.rkt\n"
                input-size outer-iteration inner-iteration)
        (ensure-gen-dir)
        (make-all-random input-size outer-iteration inner-iteration))]
     [(equal? what? 'fasl)
      (let ([fname (format "~a/use-fasl-~a-~a-~a-~a.rkt"
                           GEN_DIR input-size (or what-kind? 'random)  outer-iteration inner-iteration)])
        (printf "Generating : ~a\n" fname)
        (ensure-gen-dir)
        (generate-fasl-test-file input-size fname what-kind? outer-iteration inner-iteration))]
     [else (printf usage)])))
