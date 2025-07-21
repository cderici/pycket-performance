#lang racket

#;(define benchmarks '(string
                     sum sumfp sumloop sumrec sumrecfp sumtail
                     tak ctak cpstak takl
                     fib fibfp fibc fft
                     diviter divrec 
                     mbrot
                     ;triangl
                     array1 pnpoly nucleic pi gcbench
                     ;deriv
                     ack paraffins
                     ;primes
                     ;nqueens
                     earley))

;(define MODES '(regular byte bni))
(define MODES '(regular unroll lift both))

;; 1 - remove the logging, without the jit parameters
;; 2 - add the jit-parameters

;; PYPYLOG=jit-log-opt,jit-backend:~a-~a.trace ~~/pycket/pycket-c~a --jit decay=0,max_unroll_loops=5 ~a-nothing.rkt &> ~~/timings/~a-~a.rst
;; PYPYLOG=jit-log-opt,jit-backend:~a-~a-~a.trace ~~/pycket/pycket-c~a ~a-nothing.rkt &>> ~~/timings/~a/~a-~a.rst

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BENCHMARKING STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "trace-log-10-times"
;; "no-trace-log-10-times"
;; "extra-params-no-trace-10-times"
(define RUN_NAME "extra-params-no-trace-10-times")

(define script-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"~a ~a START\" >> ~~/benchmark-status

for i in `seq 1 10`;
do	
	~~/pycket/pycket-c --jit decay=0,max_unroll_loops=5~a ~a-nothing.rkt &>> ~~/timings/~a/~a-~a.rst
done

echo \"~a ~a DONE\" >> ~~/benchmark-status ")


(define racket-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"~a racket START\" >> ~~/benchmark-status

racket ~a-nothing.rkt &>> ~~/timings/~a-racket.rst

echo \"~a racket DONE\" >> ~~/benchmark-status ")

(define (gen-racket bench)
  (format racket-template bench bench bench bench))

(define (gen-script bench mode)
  (format script-template bench mode (if (memv mode '(byte bni)) " -b" "") bench RUN_NAME bench mode bench mode))

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
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
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

(define benchmarks '(string
                     sum sumfp sumloop sumrec sumrecfp ;sumtail
                     tak ctak cpstak takl
                     fib fibfp fibc fft
                     diviter divrec 
                     mbrot
                     ;triangl
                     array1 pnpoly nucleic pi gcbench
                     ;deriv
                     ack paraffins
                     ;primes
                     nqueens
                     ;earley
                     ))

(define modes '(-pycket -unroll -lift -both))

(define exp-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a \" >> ~~/new-setup/experiment-status

for i in `seq 1 20`;
do	
	PYPYLOG=jit-log-opt,jit-backend:~a-~a.trace ~~/pycket/pycket-c ~a~a.rkt
done

echo \"DONE ===> ~a ~a \" >> ~~/new-setup/experiment-status "
)

; &>> ~~/new-setup/timings/cleans/~a~a.rst

(define master-script-name "run-all.sh")

(call-with-output-file master-script-name
  (lambda (op)
    (display "#!/bin/bash \n\n" op)
    (for ([b benchmarks])
      (for ([m modes])
        (let* ([bash-script-name (format "~a~a.sh" b m)]
               [regmod (if (eq? m '-pycket) "" m)])

          (call-with-output-file bash-script-name
            (lambda (op)
              (display (format exp-template b m b m b regmod b m b m) op))
            #:exists 'replace)
          
          (displayln (format "qsub ~a" bash-script-name) op)))))
  #:exists 'replace)

;; activate all of them
(system "chmod 755 *.sh")
