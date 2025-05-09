;;; NQUEENS -- Compute number of solutions to 8-queens problem.
#lang racket/base
#;(require "both.rkt" "conf.rkt")

(define trace? #f)
(require "conf.rkt")
(define outer 10) ;20)
;(define nqueens-iters 1) ;7500)

(define (nqueens n)

  (define (_1-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))

  (define (my-try x y z)
    (if (null? x)
      (if (null? y)
          (begin (when trace? (begin (write z) (newline))) 1)
        0)
      (+ (if (ok? (car x) 1 z)
           (my-try (append (cdr x) y) '() (cons (car x) z))
           0)
         (my-try (cdr x) (cons (car x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

  (my-try (_1-to n) '() '()))

(define (main)
  (do ([i 1 (add1 i)])
      ((> i outer) (void))
    (time (do ([i 1 (add1 i)])
              ((> i nqueens-iters) (void))
            (nqueens 8)))))

(main)
