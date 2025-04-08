#lang racket/base

(require racket/cmdline
         racket/path)

(provide (all-defined-out))

(define GEN_DIR "./generated")

(define (generate-branchy-test-file file-path gen-function input-size outer-iteration inner-iteration)
  ;; our file will require branchy.rkt
  ;; so make sure it's there
  (let ([target-path (build-path (path-only file-path) "branchy.rkt")])
    (unless (file-exists? target-path)
      (copy-file (build-path "private" "branchy.rkt") target-path)))

  (with-output-to-file file-path
    (lambda ()
      (printf "#lang racket/base

(require \"branchy.rkt\")

(define input '~a)

(printf \"---- LOOP BEGINS ---- result : ~~a\\n\" (branchy-function input))
(for ([i (in-range ~a)])
  (time (for ([j (in-range ~a)])
          (branchy-function input))))
(printf \"---- LOOP ENDS ---- \\n\")" (build-list input-size gen-function) outer-iteration inner-iteration))
#:exists 'replace))

(define (make-all-random input-size outer-iteration inner-iteration)
  (generate-branchy-test-file (format "~a/random38-~a-~a-~a.rkt" GEN_DIR input-size outer-iteration inner-iteration)
                 (lambda (x) (random 38)) ;; specific to function in simpler-branchy2
                 input-size outer-iteration inner-iteration))

(define (make-all-same what? input-size outer-iteration inner-iteration)
  (generate-branchy-test-file (format "~a/all-~as-~a-~a-~a.rkt" GEN_DIR what? input-size outer-iteration inner-iteration)
                 (lambda (x) what?)
                 input-size outer-iteration inner-iteration))

