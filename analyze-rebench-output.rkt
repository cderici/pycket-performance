#lang racket

(require 2htdp/batch-io racket/string racket/pretty
	 "control-room/gen-scripts.rkt")

(provide rst-file->time-list)


#;(define benchmarks (list 'ack ;-- ready -- OK
                         'array1 ;-- ready -- OK
                         'cpstak ;-- ready -- OK
                         'ctak ;-- ready -- OK 
                         'diviter ;-- ready -- OK
                         'divrec ;-- ready -- OK
                         'fft ;-- read -- OK
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'fibc ;-- ready -- OK
                         'fibfp ;-- ready -- OK
                         'fib ;-- ready -- OK
                         'gcbench ;-- ready -- OK 
                         'mbrot ;-- ready -- OK
                         'nqueens ;-- ready -- OK
                         'nucleic ;-- ready -- OK 
                         'paraffins ;-- ready -- OK
                         'pi ;-- ready -- OK
                         'pnpoly ;-- ready -- OK
                         'string ;-- ready -- OK
                         'sumfp ;-- ready -- OK
                         'sumloop ;-- ready -- OK
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;'sumrecfp ;-- ready -- Ok
                         'sumrec ;-- ready -- OK
                         'sum ;-- ready -- OK
                         'takl ;-- ready -- OK
                         'tak ;-- ready -- OK
                         'perm9 ;-- ready -- OK
                         'gcold ;-- ready -- OK
                         'deriv ;-- ready -- OK
                         'primes ;-- ready -- OK
                         'ray ;-- ready -- OK 
                         'puzzle ;-- ready -- OK
                         'triangl ;-- ready -- OK
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;'sum1 ;-- ready -- OK
                         'wc ;-- ready -- OK 
                         'cat ;-- ready -- OK 
                         'graphs ;-- ready -- OK
                         'tail ;-- ready -- OK 
                         'nboyer ;-- ready -- OK
                         'earley ;-- ready -- OK
                         'sboyer ;-- ready -- OK
                         'mazefun ;-- ready -- OK
                         'simplex ;-- ready -- OK 
                         ))

(define dir-to-timings "../karst-100-in-process")

(define (get-times file-lst)
  (map (λ (s) (cadr (string-split s " ")))
       (filter (λ (s) (string-contains? s "RESULT-total:")) file-lst)))

;; .rst is the extension for rebench output file (contains stdout&stderr)

(define (create-times-lists dir-to-timings)
  (for ([bench benchmarks])
    (let* ([b-name (symbol->string bench)]
           [b-reg-file (format "~a/~a-regular.rst" dir-to-timings b-name)]
           [b-byte-file (format "~a/~a-byte.rst" dir-to-timings b-name)]
           [b-bni-file (format "~a/~a-bni.rst" dir-to-timings b-name)]

           [b-reg-out-file (format "~a/~a-regular.list" dir-to-timings b-name)]
           [b-byte-out-file (format "~a/~a-byte.list" dir-to-timings b-name)]
           [b-bni-out-file (format "~a/~a-bni.list" dir-to-timings b-name)]

           [f-reg (read-lines b-reg-file)]
           [f-byte (read-lines b-byte-file)]
           [f-bni (read-lines b-bni-file)]

           [reg-times (get-times f-reg)]
           [byte-times (get-times f-byte)]
           [bni-times (get-times f-bni)]
           )
      (begin
        (call-with-output-file b-reg-out-file
          (λ (op)
            (for ([i reg-times])
              (displayln i op)))
          #:exists 'replace)
        (call-with-output-file b-byte-out-file
          (λ (op)
            (for ([i byte-times])
              (displayln i op)))
          #:exists 'replace)
        (call-with-output-file b-bni-out-file
          (λ (op)
            (for ([i bni-times])
              (displayln i op)))
          #:exists 'replace)
        #t))))


(define (get-results-one-line result-line)
  (let*
      ([cpu-str (regexp-match #px"RESULT-cpu: (\\d*)[.](\\d*)" result-line)]
       [gc-str (regexp-match #px"RESULT-gc: (\\d*)[.](\\d*)" result-line)]
       [total-str (regexp-match #px"RESULT-total: (\\d*)[.](\\d*)" result-line)]
       [cpu (cadr (string-split (car cpu-str) " "))]
       [gc (cadr (string-split (car gc-str) " "))]
       [total (cadr (string-split (car total-str) " "))])
    (values cpu gc total)))

(define (get-results lines rst-file-str)
  (begin
    (when (> (length lines) 100)
      (displayln (format "WARNING : we got ~a results from : ~a" (length lines) rst-file-str) (current-output-port)))
    (map (lambda (line) (let-values ([(c g t) (get-results-one-line line)]) (string->number t))) (take lines 20))))

(define (rst-file->time-list rst-file)
  (let* ([f (read-file rst-file)])
    (let ([results (filter (lambda (l) (string-contains? l "RESULT")) (string-split f "\n\n"))])
      (if (= (length results) 1)
          ;; then (car results) is a giant string with multiple RESULTS
          ;; we have to split those results and get the triplets
          (let* ([rst-all-lines (string-split (car results) "\n")]
                 [triplets (for/list ([i (range (/ (length rst-all-lines) 3))])
                             (let ([f (take rst-all-lines 3)])
                               (begin
                                 (set! rst-all-lines (drop rst-all-lines 3))
                                 f)))])
            (get-results (map (lambda (triple) (string-join triple "\n")) triplets) rst-file))
          (get-results results rst-file)))))

(define (just-get-the-avg rst-file)
  (let ((ls (rst-file->time-list rst-file)))
    (/ (apply + ls) (length ls))))

(define (rst-file->time-file rst-file list-file)
  (let ([ls (rst-file->time-list rst-file)])
    (call-with-output-file list-file
      (lambda (op) 
        (for ([r ls])
          (displayln r op)))
      #:exists 'replace)))      

(define (double-digit num)
  (if (< num 10)
      (format "0~a" num)
      (number->string num)))

(define (one-result->R-input bench-name mode-str result-line)
  (let-values ([(cpu gc total) (get-results-one-line result-line)])
    (let ([dt (seconds->date (current-seconds))])
      (string-append
       (format "[2016-06-26T~a:~a:~a]\t~a00000\tms\tcpu\t~a\t~a\tCrossBenchmarks \t0\t1\t\tnothing\n"
               (double-digit (date-hour dt)) (double-digit (date-minute dt)) (double-digit (date-second dt)) cpu bench-name mode-str)
       (format "[2016-06-26T~a:~a:~a]\t~a00000\tms\tgc\t~a\t~a\tCrossBenchmarks \t0\t1\t\tnothing\n"
               (double-digit (date-hour dt)) (double-digit (date-minute dt)) (double-digit (date-second dt)) gc bench-name mode-str)
       (format "[2016-06-26T~a:~a:~a]\t~a00000\tms\ttotal\t~a\t~a\tCrossBenchmarks \t0\t1\t\tnothing\n"
               (double-digit (date-hour dt)) (double-digit (date-minute dt)) (double-digit (date-second dt)) total bench-name mode-str)))))


(define (results->R-input results bench-name mode-str rst-file)
  (begin
    (when (< (length results) 100)
      (displayln (format "WARNING : we got ~a results from : ~a" (length results) rst-file) (current-output-port)))
    (apply string-append (map (curry one-result->R-input bench-name mode-str) (take results 100)))))
    
(define (rst-file->R-input bench-name sys-str rst-file)
  (let* ([f (read-file rst-file)]
         [results (filter (λ (line) (string-contains? line "RESULT"))
                          (string-split f "\n\n"))])
    (if (= (length results) 1)
        ;; then (car results) is a giant string with multiple RESULTS
        ;; we have to split those results and get the triplets
        (let* ([rst-all-lines** (string-split (car results) "\n")]
               [rst-all-lines (filter (λ (line) (string-contains? line "RESULT")) rst-all-lines**)]
               [triplets (for/list ([i (range (/ (length rst-all-lines) 3))])
                           (let ([f (take rst-all-lines 3)])
                             (begin
                               (set! rst-all-lines (drop rst-all-lines 3))
                               f)))])
          (results->R-input (map (lambda (triple) (string-join triple "\n")) triplets) bench-name sys-str rst-file))
        (results->R-input results bench-name sys-str rst-file))))

(define rackets '((new-racket . "RacketNEW") (old-racket . "Racket")))

; new-pycket with warmup / old-pycket with warmup
(define pyckets '((new-pycket . "PycketNEW") (old-pycket . "Pycket")))

(define old-racket-pycket '((old-racket . "RacketOLD") (old-pycket . "PycketOLD")))

(define (generate-final-R-input directory)
  (let ([systems pyckets])
    (for/fold ([total-str ""])
              ([sys (map car systems)])
      (string-append total-str
                     (for/fold ([inner-str ""])
                               ([bench benchmarks])
                       (let ([file-name (if (or (equal? sys 'old-pycket)
                                                (equal? sys 'new-pycket))
                                            (format "~a/~a-~a-with-warmup.rst" directory sys bench)
                                            (format "~a/~a-~a.rst" directory sys bench))]
                             [sys-str (cdr (assv sys systems))])
                         (string-append inner-str (rst-file->R-input bench sys-str file-name))))))))

(define (produce-R-input-file results-dir file-name)
  (let ([str-to-be-written
         (string-append
          "#!/N/u/cderici/Karst/.local/bin/rebench -N -d -v /N/u/cderici/Karst/pycket-bench/rebench.conf\n"
          (generate-final-R-input results-dir))])
    (call-with-output-file file-name
      (λ (op) (display str-to-be-written op))
      #:exists 'replace)))

#;(produce-R-input-file "timings-racket" "rackets.data")

(produce-R-input-file "timings-pycket" "pyckets.data")

#;(produce-R-input-file "old-racket-pycket" "old.racket.pycket.data")

;(produce-R-input-file "karst.data")

;(define f (read-file "all-karst/gcbench-regular.rst"))

#;(define r (filter (lambda (l) (string-contains? l "RESULT")) (string-split f "\n\n")))
