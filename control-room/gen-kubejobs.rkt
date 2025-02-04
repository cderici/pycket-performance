#lang racket

(provide benchmarks)

(define benchmarks (list
                      'ack
		      'array1
                      'cpstak
                      'ctak
                      'diviter
                      'divrec
                      'fft
                      ;'fibc
                      'fibfp
                      'fib
                      'gcbench
                      'mbrot
                      'nqueens
                      ; 'nucleic ;;;;; gotta debug
                      'paraffins
                      'pi
                      'pnpoly
                      'string
                      'sumfp
                      'sumloop
                      ;'sumrecfp
                      'sumrec
                      'sum
                      'takl
                      'tak
                      'perm9
                      'gcold
                      'deriv
                      'primes
                      'ray
                      'puzzle
                      'triangl
                      ; 'sum1
                      'wc
                      'cat
                      'graphs
                      'tail
                      'nboyer
                      'earley
                      'sboyer
                      'mazefun
                      'simplex
                     ))

;; The default number of outer iterations for the benchmarks
;; running with NO warmup (for loop in the script)
(define OUTER-ITERATIONS 500)

;; 1 - remove the logging, without the jit parameters
;; 2 - add the jit-parameters

;; PYPYLOG=jit-log-opt,jit-backend:~a-~a.trace ~~/pycket/pycket-c~a --jit decay=0,max_unroll_loops=5 ~a-nothing.rkt &> ~~/timings/~a-~a.rst
;; PYPYLOG=jit-log-opt,jit-backend:~a-~a-~a.trace ~~/pycket/pycket-c~a ~a-nothing.rkt &>> ~~/timings/~a/~a-~a.rst



;; "trace-log-10-times"
;; "no-trace-log-10-times"
;; "extra-params-no-trace-10-times"

; (define RUN_NAME "extra-params-no-trace-10-times")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; GENERATOR FOR BASH SCRIPTS ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pycket-variant is either "new" or "old"
;; with/no-warmup is either "with" or "no"
;; gen-traces? is a boolean
(define (preamble pycket/racket with/no-warmup gen-traces?)
  (let ([base-pre "#!/bin/bash

NFS_SHARE=/mnt/nfs_share
PYCKET_DIR=/opt/pycket
BENCH_DIR=$NFS_SHARE/benchmarks"])
    (if gen-traces?
      (format "~a
TRACES_DIR=$BENCH_DIR/traces
SOURCE_DIR=$BENCH_DIR/src/for-traces
OUTPUT_DIR=$BENCH_DIR/timings-traces
BINARY_DIR=$PYCKET_DIR

" base-pre)
      (format "~a
SOURCE_DIR=$BENCH_DIR/src/~a-warmup
OUTPUT_DIR=$BENCH_DIR/timings-~a
BINARY_DIR=~a

" base-pre with/no-warmup pycket/racket (if (equal? pycket/racket "racket") "$PYCKET_DIR/racket/bin" "$PYCKET_DIR")))))

(define (log-line old/new pycket/racket bench-name with/no-warmup gen-traces? started/completed)
  (let ([warmup/traces (if gen-traces? "traces" (format "~a-warmup" with/no-warmup))])
    (if (equal? pycket/racket "racket")
        (format "echo \"~a ~a - `date '+%Y-%m-%d %H:%M:%S'` - ~a on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status\n\n"
            pycket/racket bench-name started/completed)
        (format "echo \"~a ~a ~a ~a - `date '+%Y-%m-%d %H:%M:%S'` - ~a on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status\n\n"
                old/new pycket/racket bench-name warmup/traces started/completed))))

(define (racket-launcher bench-name _1 _2 _3)
  (format "$BINARY_DIR/racket $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/racket-~a.rst\n\n"
                bench-name bench-name))

;; pycket-variant is either "new" or "old"
;; with/no-warmup is either "with" or "no"
;; gen-traces? is a boolean
(define (pycket-launcher bench-name pycket-variant with/no-warmup gen-traces?)
  #;(when (and (equal? with/no-warmup "no") gen-traces?)
    (error 'pycket-launcher "Cannot generate traces without warmup"))
  (let ([pycket-binary (if (equal? pycket-variant "new")
                            "pycket-c-linklets"
                            "pycket-c")]
        [time-output-file
          (if gen-traces?
            (format "~a-pycket-~a-traces-~a-warmup.rst" pycket-variant bench-name with/no-warmup)
            (format "~a-pycket-~a-~a-warmup.rst" pycket-variant bench-name with/no-warmup))])
    (if (equal? with/no-warmup "no")
      ;; no warmup -- single run in a for loop
      (if (not gen-traces?)
          (format "
for i in `seq 1 ~a`;
do
  $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a
done\n\n" OUTER-ITERATIONS pycket-binary bench-name time-output-file)
          ;; If we want traces in a no-warmup setup, then we have to get only the last one
          ;; We don't want to get OUTER-ITERATIONS many trace files (each >100M)
          (format "
for i in `seq 1 ~a`;
do
  $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a
done

PYPYLOG=jit-log-opt,jit-backend,jit-summary:$TRACES_DIR/~a-~a-~a-warmup.trace $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a

\n\n" OUTER-ITERATIONS pycket-binary bench-name time-output-file
                       pycket-binary bench-name with/no-warmup
                       pycket-binary bench-name time-output-file
                       )
          )


      ;; with warmup -- multiple runs within the benchmark source
      (if gen-traces?
        (format "PYPYLOG=jit-log-opt,jit-backend,jit-summary:$TRACES_DIR/~a-~a.trace $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a\n\n"
                pycket-variant bench-name pycket-binary bench-name time-output-file)
        (format "$BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a\n\n"
                pycket-binary bench-name time-output-file))
    )))


(struct kubejob-config (bench-name script/job pycket/racket old/new with-warmup? gen-traces? docker-image))

(define (make-config bench-name script/job pycket/racket old/new with-warmup? gen-traces? [docker-image #f])
  (kubejob-config bench-name script/job pycket/racket old/new with-warmup? gen-traces? docker-image))


;; Takes a benchmark config and produces two values
;; 1. the path of the script file
;; 2. the script content
(define (gen-script config)
  (let ([bench-name (kubejob-config-bench-name config)]
        [pycket/racket (kubejob-config-pycket/racket config)]
        [old/new (kubejob-config-old/new config)]
        [with-warmup? (kubejob-config-with-warmup? config)]
        [generate-traces? (kubejob-config-gen-traces? config)])
    (let* ([launch-function
            (if (equal? pycket/racket "racket")
                racket-launcher
                pycket-launcher)]
           [with/no-warmup (if with-warmup? "with" "no")])
      (let-values ([(file-path extension) (generate-file-path config)])
        (values (format "scripts/~a~a" file-path extension)
                (format "~a~a~a~a"
                  (preamble pycket/racket with/no-warmup generate-traces?)
                  (log-line old/new pycket/racket bench-name with/no-warmup generate-traces? "STARTED")
                  (launch-function bench-name old/new with/no-warmup generate-traces?)
                  (log-line old/new pycket/racket bench-name with/no-warmup generate-traces? "COMPLETED")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; GENERATOR FOR KUBERNETES JOBS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-file-path config)
  (let ([bench-name (kubejob-config-bench-name config)]
        [script/job (kubejob-config-script/job config)]
        [pycket/racket (kubejob-config-pycket/racket config)]
        [old/new (kubejob-config-old/new config)]
        [with-warmup? (kubejob-config-with-warmup? config)]
        [generate-traces? (kubejob-config-gen-traces? config)])
    (let* ([extension (if (equal? script/job "scripts") ".sh" ".yaml")]
           [with/no-warmup (if with-warmup? "with-warmup" "no-warmup")]
           [fname-template (if generate-traces? "~a-pycket-~a-~a-traces" "~a-pycket-~a-~a")])
      (values
        (if (equal? pycket/racket "racket")
            (format "racket-~a" bench-name)
            (format fname-template old/new bench-name with/no-warmup))
        extension))))

(define job-template
"apiVersion: batch/v1
kind: Job
metadata:
  name: ~a
spec:
  template:
    spec:
      containers:
      - name: ~a
        image: ~a
        command: [\"/bin/sh\", \"-c\"]
        args: [\"/mnt/nfs_share/benchmarks/scripts/~a\"]
        env:
          - name: POD_NAME
            valueFrom:
              fieldRef:
                fieldPath: metadata.name
        volumeMounts:
        - name: nfs-volume
          mountPath: /mnt/nfs_share
      restartPolicy: Never
      volumes:
      - name: nfs-volume
        persistentVolumeClaim:
          claimName: nfs-pvc"
)

(define (gen-job config)
  (let ([bench-name (kubejob-config-bench-name config)]
        [pycket/racket (kubejob-config-pycket/racket config)]
        [old/new (kubejob-config-old/new config)]
        [with-warmup? (kubejob-config-with-warmup? config)]
        [generate-traces? (kubejob-config-gen-traces? config)]
        [docker-image (kubejob-config-docker-image config)])
    (let-values ([(file-path extension) (generate-file-path config)])
      (let* ([with/no-warmup (if (kubejob-config-with-warmup? config) "with-warmup" "no-warmup")]
            [jname-template (if generate-traces? "~a-~a-~a-~a-traces" "~a-~a-~a-~a")]
            [job-name
              (if (equal? pycket/racket "racket")
                  (format "racket-~a" bench-name)
                  (format jname-template old/new pycket/racket bench-name with/no-warmup))])
        (values (format "jobs/~a~a" file-path extension)
                (format job-template
                  job-name job-name docker-image (string-append file-path ".sh")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN GENERATOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; benchmarks is a list of benchmark names
;; script/job is either "scripts" or "jobs"
;; pycket/racket is either "racket" or "pycket"
;; old/new is either "old" or "new"
;; with-warmup? is a boolean
;; gen-traces? is a boolean
;; gen-func is a function that takes a kubejob-config and produces a either a script or job.
;;   gen-func should return two values: the path of the file generated and the file contents
(define (generate benchmarks script/job pycket/racket old/new with-warmup? gen-traces? gen-func [DOCKER-IMAGE #f])
  (for ([b (in-list benchmarks)])
    (let ([config (make-config b script/job pycket/racket old/new with-warmup? gen-traces? DOCKER-IMAGE)])
      (let-values ([(path content) (gen-func config)])
            (call-with-output-file path
              (lambda (bop)
                (display content bop))
              #:exists 'replace)
            #;(displayln (format "generating: ~a -- with contents:\n\n~a\n"
                              path content))))))

(module+ main
  (require racket/cmdline)

  (define pycket/racket #f) ;; "racket" or "pycket"
  (define old/new "new") ;; "old" or "new"
  (define with-warmup? #f)
  (define generate-traces? #f)

  (define master-script-name "kube-apply-all-jobs.sh")
  (define racket-master-script "run-rackets.sh")

  (define gen-scripts #f)
  (define gen-jobs #f)

  (command-line
   #:once-each
   [("-s" "--scripts") "generate scripts" (set! gen-scripts #t)]
   [("-j" "--jobs") "generate kubernetes jobs" (set! gen-jobs #t)]
   #:once-any
   [("-r" "--racket") "generate racket scripts" (set! pycket/racket "racket") (set! with-warmup? #t)]
   [("-p" "--pycket") "generate pycket scripts" (set! pycket/racket "pycket")]
   #;[("--run-all-script") "generate the run-all.sh script for the sh files in the directory" (set! sys 'runall-script)]
   #:once-any
   [("-o" "--old") "old" (set! old/new "old")]
   [("-n" "--new") "new" (set! old/new "new")]
   #:once-any
   [("--with-warmup") "with warmup (for pycket)" (set! with-warmup? #t)]
   [("--no-warmup") "without warmup (for pycket)" (set! with-warmup? #f)]
   [("-t" "--traces") "with warmup, extract the JIT log" (set! generate-traces? #t)]
   #:args ([docker-image #f])

  ;; Validate arguments
  (unless (or gen-scripts gen-jobs)
    (error 'main "Please specify either --scripts or --jobs"))
  (unless (memv pycket/racket '("pycket" "racket"))
      (error 'gen-script "system must be either \"pycket\" or \"racket\""))
  (unless (memv old/new '("old" "new"))
      (error 'gen-script "old/new must be either \"old\" or \"new\""))
  (unless (boolean? with-warmup?)
      (error 'gen-script "with-warmup? must be a boolean"))
  (unless (boolean? generate-traces?)
      (error 'gen-script "generate-traces? must be a boolean"))

  (define script/job (if gen-scripts "scripts" "jobs"))
  ;; Generate stuff
  (when gen-scripts
    (generate benchmarks script/job pycket/racket old/new with-warmup? generate-traces? gen-script))

  (when gen-jobs
    (generate benchmarks script/job pycket/racket old/new with-warmup? generate-traces? gen-job docker-image))

  (let ([traces/warmup (if generate-traces? "traces" (format "~a-warmup" (if with-warmup? "with" "no")))])
    (if (equal? pycket/racket "racket")
      (printf "\nDONE GENERATING ~a ~a\n\n" pycket/racket script/job)
      (printf "\nDONE GENERATING ~a ~a ~a ~a\n\n" pycket/racket old/new script/job traces/warmup)))

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

   (when gen-scripts
    (and (system "chmod 755 scripts/*.sh") (void)))
   ))
