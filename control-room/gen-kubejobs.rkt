#lang racket

(provide benchmarks)

#;(define benchmarks (list 'ack ;-- ready -- OK
		         'array1 ;-- ready -- OK
                         'cpstak ;-- ready -- OK
                     	 'ctak ;-- ready -- OK
                     	 'diviter ;-- ready -- OK
                     'divrec ;-- ready -- OK
                     'fft ;-- read -- OK
                     ;'fibc ;-- ready -- OK
                     'fibfp ;-- ready -- OK
                     'fib ;-- ready -- OK
                     'gcbench ;-- ready -- OK -------------------------------
                     'mbrot ;-- ready -- OK
                     'nqueens ;-- ready -- OK
                     'nucleic ;-- ready -- OK
                     'paraffins ;-- ready -- OK
                     'pi ;-- ready -- OK
                     'pnpoly ;-- ready -- OK
                     'string ;-- ready -- OK
                     'sumfp ;-- ready -- OK
                     'sumloop ;-- ready -- OK
                     ;'sumrecfp ;-- ready -- Ok
                     'sumrec ;-- ready -- OK
                     'sum ;-- ready -- OK
                     'takl ;-- ready -- OK
                     'tak ;-- ready -- OK
                     'perm9 ;-- ready -- OK
                     'gcold ;-- ready -- OK
                     'deriv ;-- ready -- OK
                     'primes ;-- ready -- OK
                     'ray ;-- ready -- OK  ------------------------------------
                     'puzzle ;-- ready -- OK
                     'triangl ;-- ready -- OK
                     ; 'sum1 ;-- ready -- OK ------------------------
                     'wc ;-- ready -- OK -----------------------------
                     'cat ;-- ready -- OK ----------------------------
                     'graphs ;-- ready -- OK
                     'tail ;-- ready -- OK ----------------------------
                     'nboyer ;-- ready -- OK
                     'earley ;-- ready -- OK
                     'sboyer ;-- ready -- OK
                     'mazefun ;-- ready -- OK
                     'simplex ;-- ready -- OK
                     ))

(define benchmarks (list
  'ack 'mbrot))


;; 1 - remove the logging, without the jit parameters
;; 2 - add the jit-parameters

;; PYPYLOG=jit-log-opt,jit-backend:~a-~a.trace ~~/pycket/pycket-c~a --jit decay=0,max_unroll_loops=5 ~a-nothing.rkt &> ~~/timings/~a-~a.rst
;; PYPYLOG=jit-log-opt,jit-backend:~a-~a-~a.trace ~~/pycket/pycket-c~a ~a-nothing.rkt &>> ~~/timings/~a/~a-~a.rst



;; "trace-log-10-times"
;; "no-trace-log-10-times"
;; "extra-params-no-trace-10-times"

; (define RUN_NAME "extra-params-no-trace-10-times")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; TEMPLATES FOR SCRIPTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define preamble
  "#!/bin/bash

NFS_SHARE=/mnt/nfs_share
BENCH_DIR=$NFS_SHARE/benchmarks
PYCKET_DIR=/opt/pycket
RACKET_DIR=$PYCKET_DIR/racket/bin
SOURCE_DIR=$NFS_SHARE/pycket-performance/src
OUTPUT_DIR=$BENCH_DIR/timings-pycket

")

(define (log-start old/new pycket/racket bench-name with-warmup?)
  (format "echo \"~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'` - STARTED on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status\n\n"
          old/new pycket/racket bench-name (if with-warmup? "with" "no")))

(define (log-done old/new pycket/racket bench-name with-warmup?)
  (format "echo \"~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'` - COMPLETED on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status\n\n"
          old/new pycket/racket bench-name (if with-warmup? "with" "no")))

(define (launch-benchmark-with-warmup binary bench-src-path output-path)
  (format "~a ~a &>> ~a\n\n"
          binary bench-src-path output-path))

(define (launch-benchmark-no-warmup binary bench-src-path output-path)
  (format "
for i in `seq 1 100`;
do
  ~a ~a &>> ~a
done\n\n
"
    binary bench-src-path output-path))

;; Racket

(define (launch-racket bench-name _)
  (launch-benchmark-with-warmup
    (format "$RACKET_DIR/racket")
    (format "$SOURCE_DIR/with-warmup/~a.rkt" bench-name)
    (format "$OUTPUT_DIR/racket-~a.rst" bench-name)))

;;;; Pycket With Warmup

;; pycket-variane is either "new" or "old"
(define (launch-pycket-with-warmup bench-name pycket-variant)
  (unless (memv pycket-variant '("new" "old"))
    (error 'launch-pycket-with-warmup "pycket-variant must be either 'new or 'old"))
  (let ([pycket-binary (if (equal? pycket-variant "new")
                           "pycket-c-linklets"
                           "pycket-c")])
    (launch-benchmark-with-warmup
      (format "$PYCKET_DIR/~a" pycket-binary)
      (format "$SOURCE_DIR/with-warmup/~a.rkt" bench-name)
      (format "$OUTPUT_DIR/~a-pycket-~a-with-warmup.rst"
        pycket-variant
        bench-name))))

(define (launch-new-pycket-with-warmup bench-name)
  (launch-pycket-with-warmup bench-name "new"))

(define (launch-old-pycket-with-warmup bench-name)
  (launch-pycket-with-warmup bench-name "old"))

;;;; Pycket No Warmup

;; pycket-variane is either "new" or "old"
(define (launch-pycket-no-warmup bench-name pycket-variant)
  (unless (memv pycket-variant '("new" "old"))
    (error 'launch-pycket-with-warmup "pycket-variant must be either 'new or 'old"))
  (let ([pycket-binary (if (equal? pycket-variant "new")
                           "pycket-c-linklets"
                           "pycket-c")])
    (launch-benchmark-no-warmup
      (format "$PYCKET_DIR/~a" pycket-binary)
      (format "$SOURCE_DIR/no-warmup/~a.rkt" bench-name)
      (format "$OUTPUT_DIR/~a-pycket-~a-no-warmup.rst"
        pycket-variant
        bench-name))))

(define (launch-new-pycket-no-warmup bench-name)
  (launch-pycket-no-warmup bench-name "new"))

(define (launch-old-pycket-no-warmup bench-name)
  (launch-pycket-no-warmup bench-name "old"))


(struct kubejob-config (bench-name pycket/racket old/new with-warmup? gen-traces?))

;; Takes a benchmark config and produces two values
;; 1. the path of the script file
;; 2. the script content
(define (gen-script config)
  (let ([bench-name (kubejob-config-bench-name config)]
        [sys (kubejob-config-pycket/racket config)]
        [old/new (kubejob-config-old/new config)]
        [with-warmup? (kubejob-config-with-warmup? config)]
        [generate-traces? (kubejob-config-with-warmup? config)])
    (unless (memv sys '("pycket" "racket"))
      (error 'gen-script "system must be either \"pycket\" or \"racket\""))
    (unless (memv old/new '("old" "new"))
      (error 'gen-script "old/new must be either \"old\" or \"new\""))
    (unless (boolean? with-warmup?)
      (error 'gen-script "with-warmup? must be a boolean"))
    (unless (boolean? generate-traces?)
      (error 'gen-script "generate-traces? must be a boolean"))
    (let ([launch-function
            (if (equal? sys "racket")
                launch-racket
                (if with-warmup?
                    launch-pycket-with-warmup
                    launch-pycket-no-warmup))]
          [file-path-str
            (format "scripts/~a-~a-~a-~a-warmup.sh"
              old/new sys bench-name (if with-warmup? "with" "no"))])
      (values file-path-str
              (format "~a~a~a~a"
                preamble
                (log-start old/new sys bench-name with-warmup?)
                (launch-function bench-name old/new)
                (log-done old/new sys bench-name with-warmup?))))))

(define script-template
  "#!/bin/bash

NFS_SHARE=/mnt/nfs_share
BENCH_DIR=$NFS_SHARE/benchmarks
PYCKET_DIR=/opt/pycket
SOURCE_DIR=$NFS_SHARE/pycket-performance/src
OUTPUT_DIR=$BENCH_DIR/timings-pycket


echo \"new pycket ack #t-warmup - `date '+%Y-%m-%d %H:%M:%S'` - STARTED on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status

$PYCKET_DIR/pycket-c-linklets $SOURCE_DIR/with-warmup/ack.rkt &>> $OUTPUT_DIR/new-pycket-ack-with-warmup.rst

echo \"new pycket ack #t-warmup - `date '+%Y-%m-%d %H:%M:%S'` - COMPLETED on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status")


#;(define racket-template
  "#!/bin/bash

cd \"$PBS_O_WORKDIR\"
echo \"~a racket START\" >> ~~/benchmark-status

racket ~a-nothing.rkt &>> ~~/timings-racket/~a-racket.rst

echo \"~a racket DONE\" >> ~~/benchmark-status ")

#;(define (gen-racket bench)
  (format racket-template bench bench bench bench))

;; RACKETS

#|
(for ([bench benchmarks])
  (call-with-output-file (format "~a-racket.sh" bench)
    (λ (op) (display (gen-racket bench) op))
    #:exists 'replace))

;; batch
(let ([file-name "run-racket.sh"]
      [scripts-str (apply string-append (map (lambda (b) (format "qsub ~a-racket.sh\n" b)) benchmarks))])
  (call-with-output-file file-name
    (lambda (op) (display (string-append "#!/bin/bash \n\n" scripts-str) op))
    #:exists 'replace))
|#

;~~/pycket/pycket-c~a ~a-nothing.rkt &> ~~/benchresults/~a-~a.rst

;; INDIVIDUAL BENCHMARKS

#;(for ([bench benchmarks])
  (for ([mode MODES])
    (call-with-output-file (format "~a-~a-~a.sh" bench RUN_NAME mode)
      (λ (op) (display (gen-script bench mode) op))
      #:exists 'replace)))

;; BATCH RUNNERS
#;(for ([mode MODES])
  (let ([file-name (format "run-~a-~a.sh" RUN_NAME mode)]
        [scripts-str (apply string-append (map (lambda (b) (format "qsub ~a-~a-~a.sh\n" b RUN_NAME mode)) benchmarks))])
    (call-with-output-file file-name
      (lambda (op) (display (string-append "#!/bin/bash \n\n" scripts-str) op))
      #:exists 'replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNROLLING STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define benchs '(fib sum sumtail sumrec tak))

(define unroll-script-template
  "#!/bin/bash

cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a unroll : ~a - in-process : ~a times\" >> ~~/benchmark-status

for i in `seq 1 20`;
do
	~~/pycket/pycket-c ~a-~a-~a-nothing.rkt &>> ~~/timings/unroll/~a-~a-~a.rst
done

echo \"DONE ===> ~a unroll : ~a - in-process : ~a times\" >> ~~/benchmark-status ")

;(define master-script-name "run-unrolls.sh")
;; one script to rule them all

#;(call-with-output-file master-script-name
  (lambda (op)
    (display "#!/bin/bash \n\n" op)
    (for ([b benchs])
      (for ([unroll '(0 1 2 3 4 5 6)])
        (for ([in-proc '(1 20)])
          (let ([file-name (format "~a-~a-~a-nothing.rkt" b unroll in-proc)]
                [bash-script-name (format "~a-~a-~a.sh" b unroll in-proc)])

            ;; (1) write the <bench>-nothing.rkt
            (call-with-output-file file-name
              (lambda (op)
                (display (format "(module ~a-~a-~a pycket/wrap nothing)" b unroll in-proc) op))
              #:exists 'replace)

            ;; (2) write the corresponding bash script
            (call-with-output-file bash-script-name
              (lambda (op)
                (display (format unroll-script-template b unroll in-proc b unroll in-proc b unroll in-proc b unroll in-proc) op))
              #:exists 'replace)

            ;; (3) write the line to call the above script in the master script
            (display (format "qsub ~a\n" bash-script-name) op)
            )))))
  #:exists 'replace)

(define traces-template
  "#!/bin/bash

cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a ~a-warmup traces - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

PYPYLOG=jit-log-opt,jit-backend,jit-summary:../traces/~a-~a.trace ~a ~a/~a.rkt &>> ../timings-traces/~a-~a-~a-traces.rst

echo \"DONE ===> ~a ~a ~a ~a-warmup traces - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

(define with-warmup-template
  "#!/bin/bash

cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

~a ~a/~a.rkt &>> ../timings-pycket/~a-~a-~a-with-warmup.rst

echo \"DONE ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

(define racket-template
  "#!/bin/bash

cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

~a ~a/~a.rkt &>> ../timings-racket/~a-~a-~a.rst

echo \"DONE ===> ~a ~a ~a - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

(define without-warmup-template
  "#!/bin/bash

cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

for i in `seq 1 100`;
do
	~a ~a/~a.rkt &>> ../timings-pycket/~a-~a-~a-no-warmup.rst
done

echo \"DONE ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

(module+ main
  (require racket/cmdline)

  (define sys #f) ;'(racket pycket))
  (define mode #f) ;'(old new))
  (define with-warmup? #f) ;'(-with-warmup -no-warmup)) ; this is only for pycket

  (define traces #f)

  (define master-script-name "kube-apply-all-jobs.sh")
  (define racket-master-script "run-rackets.sh")

  (command-line
   #:once-any
   [("--racket") "generate racket scripts" (set! sys "racket")]
   [("--pycket") "generate pycket scripts" (set! sys "pycket")]
   #;[("--run-all-script") "generate the run-all.sh script for the sh files in the directory" (set! sys 'runall-script)]
   #:once-any
   [("--old") "old" (set! mode "old")]
   [("--new") "new" (set! mode "new")]
   #:once-any
   [("--with-warmup") "with warmup (for pycket)" (set! with-warmup? #t)]
   [("--no-warmup") "without warmup (for pycket)" (set! with-warmup? #f)]
   [("--traces") "with warmup, extract the JIT log" (set! with-warmup? #t) (set! traces #t)]

   #:args ()
   (printf "\nGENERATING ~a jobs\nMODE : ~a\nwith-warmup? : ~a\n\n" sys mode with-warmup?)

   #;(when traces
      (unless (equal? sys 'pycket)
	 	    (error 'gen-scripts "system has to be pycket to generate JIT backend logs")))

   #;(when (equal? sys 'racket) (set! with-warmup? #t))

   (for ([b (in-list benchmarks)])
     (let* ([config (kubejob-config b sys mode with-warmup? traces)]
            [bash-script-name (if (equal? sys 'racket)
     	   		      	  (format "~a-~a-~a.sh" mode sys b)
              (if traces
                  (format "~a-~a-~a-traces.sh" mode sys b)
                  (format "~a-~a-~a-~a-warmup.sh" mode sys b (if with-warmup? "with" "no"))))]
            #;[command (if (equal? sys 'racket)
                         (if (equal? mode 'old) (format "~~/racketland/racket/bin/racket")
			     	       	     (format "~~/racketland/racket/racket/bin/racket"))
                         (if (equal? mode 'new) "~/pycketland/pycket/pycket-c-linklets" "~/pycketland/pycket/pycket-c"))]
            #;[src-dir (if traces
	    	     	 "../src/for-traces"
	    	         (if with-warmup?
                             "../src/with-warmup"
                             "../src/without-warmup"))])
        (let-values ([(script-path script-content) (gen-script config)])
          #;(call-with-output-file script-path
            (lambda (bop)
              (display script-content bop))
            #:exists 'replace)
          (displayln (format "generating script: ~a -- with contents:\n~a\n"
                             script-path script-content)))))

   ;; submit all script
   #;(call-with-output-file master-script-name
     (lambda (op)
       (display "#!/bin/bash \n\n" op)
       (for ([p (directory-list)])
         (when (path-has-extension? p #".sh")
           (let ([p-str (path->string (file-name-from-path p))])
             (unless (string-contains? p-str "run-all")
               (displayln (format "qsub ~a" p) op))))))
     #:exists 'replace)

   #;(system "chmod 755 *.sh")
   ))
