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
                      ; 'sumrecfp
                      ; 'sumrec
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

(define PYCKET-INTERNAL "P")
(define RACKET-INTERNAL "R")
(define WITH-WARMUP-INTERNAL "WW")
(define NO-WARMUP-INTERNAL "NW")

(define PYCKET-HUMAN "pycket")
(define RACKET-HUMAN "racket")
(define WITH-WARMUP-HUMAN "with-warmup")
(define NO-WARMUP-HUMAN "no-warmup")

(define OLD-INTERNAL "O")
(define OLD-HUMAN "old")
(define NEW-INTERNAL "N")
(define NEW-HUMAN "new")

(define TRACES-INTERNAL "T")
(define TRACES-HUMAN "traces")

(struct repr (internal human))
(define PYCKET-REPR (repr PYCKET-INTERNAL PYCKET-HUMAN))
(define RACKET-REPR (repr RACKET-INTERNAL RACKET-HUMAN))
(define w-warmup-repr (repr WITH-WARMUP-INTERNAL WITH-WARMUP-HUMAN))
(define no-warmup-repr (repr NO-WARMUP-INTERNAL NO-WARMUP-HUMAN))
(define new-repr (repr NEW-INTERNAL NEW-HUMAN))
(define old-repr (repr OLD-INTERNAL OLD-HUMAN))
(define TRACES-REPR (repr TRACES-INTERNAL TRACES-HUMAN))

(define (new/old-repr is-new?)
  (repr-internal (if is-new? new-repr old-repr)))

(define (warmup-repr with-warmup?)
  (repr-internal (if with-warmup? w-warmup-repr no-warmup-repr)))

(define (warmup-human-repr with-warmup?)
  (repr-human (if with-warmup? w-warmup-repr no-warmup-repr)))

(define (pycket/racket-human-repr is-pycket?)
  (repr-human (if is-pycket? PYCKET-REPR RACKET-REPR)))

(define (pycket/racket-internal-repr is-pycket?)
  (repr-internal (if is-pycket? PYCKET-REPR RACKET-REPR)))

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

;; all inputs are bool
(define (preamble is-pycket? with-warmup? generate-traces?)
  (when (and generate-traces? (not is-pycket?))
    (error 'preamble "something is wrong, racket and generate-traces? can't be both true"))
  (let ([base-pre "#!/bin/bash

NFS_SHARE=/mnt/nfs_share
PYCKET_DIR=/opt/pycket
BENCH_DIR=$NFS_SHARE/benchmarks"])
    (if generate-traces?
      ;; script to generate traces
      (format "~a
TRACES_DIR=$BENCH_DIR/traces
SOURCE_DIR=$BENCH_DIR/src/~a
OUTPUT_DIR=$BENCH_DIR/timings-traces
BINARY_DIR=$PYCKET_DIR

" base-pre (warmup-human-repr with-warmup?))
      ;; no traces, regular script
      (format "~a
SOURCE_DIR=$BENCH_DIR/src/~a
OUTPUT_DIR=$BENCH_DIR/timings-~a
BINARY_DIR=~a

" base-pre
  (warmup-human-repr with-warmup?)
  (pycket/racket-human-repr is-pycket?)
  (if is-pycket? "$PYCKET_DIR" "$PYCKET_DIR/racket/bin")))))

(define (log-line is-new? is-pycket? bench-name with-warmup? gen-traces? started/completed)
  (let ([warmup/traces (if gen-traces? TRACES-INTERNAL (warmup-repr with-warmup?))]
        [pycket/racket (pycket/racket-internal-repr is-pycket?)])
    (if (not is-pycket?)
        ;; racket
        (format "echo \"~a ~a - `date '+%Y-%m-%d %H:%M:%S'` - ~a on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status\n\n"
            pycket/racket bench-name started/completed)
        ;; pycket
        (format "echo \"~a~a ~a ~a - `date '+%Y-%m-%d %H:%M:%S'` - ~a on pod: $POD_NAME\" >> $BENCH_DIR/experiment-status\n\n"
                (new/old-repr is-new?) pycket/racket
                warmup/traces bench-name
                started/completed))))

(define (racket-script bench-name _1 _2 _3)
  (let ([racket-binary "racket"])
    (format "$BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a-~a.rst\n\n"
            racket-binary bench-name (repr-internal RACKET-REPR) bench-name)))

;; pycket-variant is either "new" or "old"
;; with/no-warmup is either "with" or "no"
;; gen-traces? is a boolean
(define (pycket-script bench-name is-new? with-warmup? gen-traces?)
  #;(when (and (equal? with/no-warmup "no") gen-traces?)
    (error 'pycket-launcher "Cannot generate traces without warmup"))
  (let* ([pycket-binary (if is-new? "pycket-c-linklets" "pycket-c")]
         ;; e.g. NP-WW (new pycket with warmup)
         [pycket-variant-repr (format "~a~a-~a"
                                      (new/old-repr is-new?) (repr-internal PYCKET-REPR)
                                      (warmup-repr with-warmup?))]
         [time-output-file-name* (format "~a-~a" pycket-variant-repr bench-name)]
         [time-output-file-name (format "~a~a.rst" time-output-file-name* (if gen-traces? "-traces" ""))])
    (if (not with-warmup?)
      ;; no warmup -- single run in a for loop
      (if (not gen-traces?)
          (format "
for i in `seq 1 ~a`;
do
  $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a
done\n\n" OUTER-ITERATIONS pycket-binary bench-name time-output-file-name)
          (format "
mkdir -p $TRACES_DIR/~a

for i in `seq 1 ~a`;
do
  PYPYLOG=jit-log-opt,jit-backend,jit-summary:$TRACES_DIR/~a/$i.trace $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a
done
\n\n" time-output-file-name* OUTER-ITERATIONS time-output-file-name*
      pycket-binary bench-name time-output-file-name
            )
          )

      ;; with warmup -- multiple runs within the benchmark source
      (if gen-traces?
        (format "PYPYLOG=jit-log-opt,jit-backend,jit-summary:$TRACES_DIR/~a.trace $BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a\n\n"
                time-output-file-name* pycket-binary bench-name time-output-file-name)
        (format "$BINARY_DIR/~a $SOURCE_DIR/~a.rkt &>> $OUTPUT_DIR/~a\n\n"
                pycket-binary bench-name time-output-file-name))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN GENERATOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-script? : bool (false -> it's a job)
;; is-pycket? : bool (false -> it's racket)
;; is-new?    : bool (false -> it's old)
(struct kubejob-config (bench-name is-script? is-pycket? is-new? with-warmup? gen-traces? docker-image))

(define (make-config bench-name is-script? is-pycket? is-new? with-warmup? gen-traces? [docker-image #f])
  (kubejob-config bench-name is-script? is-pycket? is-new? with-warmup? gen-traces? docker-image))


;; benchmarks is a list of benchmark names
;; is-script? : bool (false -> it's a job)
;; is-pycket? : bool (false -> it's racket)
;; is-new?    : bool (false -> it's old)
;; with-warmup? is a boolean
;; gen-traces? is a boolean
;; gen-func is a function that takes a kubejob-config and produces a either a script or job.
;;   gen-func should return two values: the path of the file generated and the file contents
(define (generate benchmarks is-script? is-pycket? is-new? with-warmup? gen-traces? gen-func [DOCKER-IMAGE #f])
  (for ([b (in-list benchmarks)])
    (let ([config (make-config b is-script? is-pycket? is-new? with-warmup? gen-traces? DOCKER-IMAGE)])
      (let-values ([(path content) (gen-func config)])
            (call-with-output-file path
              (lambda (bop)
                (display content bop))
              #:exists 'replace)
            #;(displayln (format "generating: ~a -- with contents:\n\n~a\n"
                              path content))))))

;; Takes a benchmark config and produces two values
;; 1. the path of the script file
;; 2. the script content
(define (gen-script config)
  (let ([bench-name (kubejob-config-bench-name config)]
        [is-pycket? (kubejob-config-is-pycket? config)]
        [is-new? (kubejob-config-is-new? config)]
        [with-warmup? (kubejob-config-with-warmup? config)]
        [generate-traces? (kubejob-config-gen-traces? config)])
    (let ([launch-function (if is-pycket? pycket-script racket-script)])
      (let-values ([(file-name extension) (generate-file-path config)])
        (values (string-append "scripts/" file-name "." extension)
                (format "~a~a~a~a"
                  (preamble is-pycket? with-warmup? generate-traces?)
                  (log-line is-new? is-pycket? bench-name with-warmup? generate-traces? "STARTED")
                  (launch-function bench-name is-new? with-warmup? generate-traces?)
                  (log-line is-new? is-pycket? bench-name with-warmup? generate-traces? "COMPLETED")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; GENERATOR FOR KUBERNETES JOBS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-file-path config)
  (let ([bench-name (kubejob-config-bench-name config)]
        [is-script? (kubejob-config-is-script? config)]
        [generate-traces? (kubejob-config-gen-traces? config)])
    (let* ([extension (if is-script? "sh" "yaml")]
           [fname-template (if generate-traces? "~a~a-~a-~a-traces" "~a~a-~a-~a")])
      (if (not (kubejob-config-is-pycket? config))
        ;; racket
        (values (format "~a-~a" (repr-internal RACKET-REPR) bench-name) extension)
        ;; pycket
        (values (format fname-template
                  (new/old-repr (kubejob-config-is-new? config))
                  (repr-internal PYCKET-REPR)
                  (warmup-repr (kubejob-config-with-warmup? config))
                  bench-name)
                extension)))))

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
        [is-pycket? (kubejob-config-is-pycket? config)]
        [is-new? (kubejob-config-is-new? config)]
        [with-warmup? (kubejob-config-with-warmup? config)]
        [generate-traces? (kubejob-config-gen-traces? config)]
        [docker-image (kubejob-config-docker-image config)])
    (let-values ([(file-name extension) (generate-file-path config)])
      (let*
        ([pycket-variant-repr (format "~a~a-~a-~a"
                                      (new/old-repr is-new?) (repr-internal PYCKET-REPR)
                                      (warmup-repr with-warmup?) bench-name)]
         [pycket-job-name (format "~a~a" pycket-variant-repr (if generate-traces? "-traces" ""))]
         [job-name
           (string-downcase
             (if (not is-pycket?)
               (format "~a-~a" (repr-internal RACKET-REPR) bench-name)
               pycket-job-name))])
          (values (string-append "jobs/" file-name "." extension)
                  (format job-template
                    job-name job-name docker-image (string-append file-name ".sh")))))))

(module+ main
  (require racket/cmdline)

  (define pycket/racket #f) ;; "racket" or "pycket"
  (define old/new "new") ;; "old" or "new"
  (define with-warmup? #f)
  (define generate-traces? #f)

  (define master-script-name "kube-apply-all-jobs.sh")
  (define racket-master-script "run-rackets.sh")

  (define is-script? #t)

  (command-line
   #:once-each
   [("-s" "--scripts") "generate scripts" (set! is-script? #t)]
   [("-j" "--jobs") "generate kubernetes jobs" (set! is-script? #f)]
   [("-t" "--traces") "with warmup, extract the JIT log" (set! generate-traces? #t)]
   #:once-any
   [("-r" "--racket") "generate racket scripts" (set! pycket/racket "racket") (set! with-warmup? #t)]
   [("-p" "--pycket") "generate pycket scripts" (set! pycket/racket "pycket")]
   #:once-any
   [("-o" "--old") "old" (set! old/new "old")]
   [("-n" "--new") "new" (set! old/new "new")]
   #:once-any
   [("--with-warmup") "with warmup (for pycket)" (set! with-warmup? #t)]
   [("--no-warmup") "without warmup (for pycket)" (set! with-warmup? #f)]
   #:args ([docker-image #f])

  ;; Validate arguments
  (unless (memv pycket/racket '("pycket" "racket"))
      (error 'pycket/racket "system must be either \"pycket\" or \"racket\""))
  (unless (memv old/new '("old" "new"))
      (error 'old/new "must be either \"old\" or \"new\""))
  (unless (boolean? with-warmup?)
      (error 'with-warmup? "must be a boolean"))
  (unless (boolean? generate-traces?)
      (error 'generate-traces? "must be a boolean"))

  (define is-new? (equal? old/new "new"))
  (define is-pycket? (equal? pycket/racket "pycket"))

  ;; Generate stuff
  (when is-script?
    (generate benchmarks is-script? is-pycket? is-new? with-warmup? generate-traces? gen-script))

  (when (not is-script?)
    (generate benchmarks is-script? is-pycket? is-new? with-warmup? generate-traces? gen-job docker-image))

  (let ([script/job (if is-script? "scripts" "jobs")])
    (if (equal? pycket/racket "racket")
      (printf "\nDONE GENERATING ~a ~a\n\n" pycket/racket script/job)
      (printf "\nDONE GENERATING ~a ~a ~a ~a ~a\n\n"
              pycket/racket old/new script/job (warmup-human-repr with-warmup?)
              (if generate-traces? (repr-human TRACES-REPR) ""))))

   (when is-script?
    (and (system "chmod 755 scripts/*.sh") (void)))
   ))
