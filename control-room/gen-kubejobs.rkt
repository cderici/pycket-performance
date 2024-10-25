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
;;;;;;;;;;;;;;;;;;;;;;;; GENERATORS FOR BASH SCRIPTS ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pycket-variant is either "new" or "old"
;; with/no-warmup is either "with" or "no"
;; gen-traces? is a boolean
(define (preamble sys with/no-warmup gen-traces?)
  (let ([base-pre "#!/bin/bash

NFS_SHARE=/mnt/nfs_share
PYCKET_DIR=/opt/pycket
BENCH_DIR=$NFS_SHARE/benchmarks"])
    (if gen-traces?
      (format "~a
TRACES_DIR=$BENCH_DIR/traces
SOURCE_DIR=$NFS_SHARE/pycket-performance/src/for-traces
OUTPUT_DIR=$BENCH_DIR/timings-traces
BINARY_DIR=$PYCKET_DIR

" base-pre)
      (format "~a
SOURCE_DIR=$NFS_SHARE/pycket-performance/src/~a-warmup
OUTPUT_DIR=$BENCH_DIR/timings-pycket
BINARY_DIR=~a

" base-pre with/no-warmup (if (equal? sys "racket") "$PYCKET_DIR/racket/bin" "$PYCKET_DIR")))))

(define (log-line old/new pycket/racket bench-name with/no-warmup gen-traces? started/completed)
  (let ([warmup/traces (if gen-traces? "traces" (format "~a-warmup" with/no-warmup))])
    (format "echo \"~a ~a ~a ~a - `date '+%Y-%m-%d %H:%M:%S'` - ~a on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status\n\n"
            old/new pycket/racket bench-name warmup/traces started/completed)))

(define (racket-launcher bench-name _1 _2 _3)
  (format "$BINARY_DIR/racket $SOURCE_DIR/with-warmup/~a.rkt &>> $OUTPUT_DIR/racket-~a.rst\n\n"
                bench-name bench-name))

;; pycket-variant is either "new" or "old"
;; with/no-warmup is either "with" or "no"
;; gen-traces? is a boolean
(define (pycket-launcher bench-name pycket-variant with/no-warmup gen-traces?)
  (when (and (equal? with/no-warmup "no") gen-traces?)
    (error 'pycket-launcher "Cannot generate traces without warmup"))
  (let ([pycket-binary (if (equal? pycket-variant "new")
                            "pycket-c-linklets"
                            "pycket-c")]
        [time-output-file
          (if gen-traces?
            (format "~a-pycket-~a-traces.rst" pycket-variant bench-name)
            (format "~a-pycket-~a-~a-warmup.rst" pycket-variant bench-name with/no-warmup))])
    (if (equal? with/no-warmup "no")
      ;; no warmup -- single run in a for loop
      (format "
for i in `seq 1 100`;
do
  $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a
done\n\n
" pycket-binary bench-name time-output-file)

      ;; with warmup -- multiple runs within the benchmark source
      (if gen-traces?
        (format "PYPYLOG=jit-log-opt,jit-backend,jit-summary:$TRACES_DIR/~a-~a.trace $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a\n\n"
                pycket-variant bench-name pycket-binary bench-name time-output-file)
        (format "$BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a\n\n"
                pycket-binary bench-name time-output-file))
    )))

(struct kubejob-config (bench-name pycket/racket old/new with-warmup? gen-traces?))

;; Takes a benchmark config and produces two values
;; 1. the path of the script file
;; 2. the script content
(define (gen-script config)
  (let ([bench-name (kubejob-config-bench-name config)]
        [sys (kubejob-config-pycket/racket config)]
        [old/new (kubejob-config-old/new config)]
        [with-warmup? (kubejob-config-with-warmup? config)]
        [generate-traces? (kubejob-config-gen-traces? config)])
    (unless (memv sys '("pycket" "racket"))
      (error 'gen-script "system must be either \"pycket\" or \"racket\""))
    (unless (memv old/new '("old" "new"))
      (error 'gen-script "old/new must be either \"old\" or \"new\""))
    (unless (boolean? with-warmup?)
      (error 'gen-script "with-warmup? must be a boolean"))
    (unless (boolean? generate-traces?)
      (error 'gen-script "generate-traces? must be a boolean"))
    (let* ([launch-function
            (if (equal? sys "racket")
                racket-launcher
                pycket-launcher)]
           [with/no-warmup (if with-warmup? "with" "no")]
           [file-prefix (if generate-traces? "traces" (format "~a-warmup" with/no-warmup))]
           [file-path-str
             (format "scripts/~a-~a-~a-~a.sh"
                 old/new sys bench-name file-prefix)])
      (values file-path-str
              (format "~a~a~a~a"
                (preamble sys with/no-warmup generate-traces?)
                (log-line old/new sys bench-name with/no-warmup generate-traces? "STARTED")
                (launch-function bench-name old/new with/no-warmup generate-traces?)
                (log-line old/new sys bench-name with/no-warmup generate-traces? "COMPLETED"))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; GENERATORS FOR KUBERNETES JOBS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (printf "\nGENERATING ~a jobs\nMODE : ~a\nwith-warmup? : ~a\ngenerate-traces? : ~a\n\n" sys mode with-warmup? traces)

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
          (call-with-output-file script-path
            (lambda (bop)
              (display script-content bop))
            #:exists 'replace)
          #;(displayln (format "generating script: ~a -- with contents:\n\n~a\n"
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

   (system "chmod 755 scripts/*.sh")
   ))
