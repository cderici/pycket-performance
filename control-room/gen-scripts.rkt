#lang racket

(provide benchmarks)

(define benchmarks (list 'ack ;-- ready -- OK
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

#;(define benchmarks (list 'mazefun 'deriv 'nqueens 'sumrec 'nucleic 'triangl 'mbrot))


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


#;(define racket-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"~a racket START\" >> ~~/benchmark-status

racket ~a-nothing.rkt &>> ~~/timings-racket/~a-racket.rst

echo \"~a racket DONE\" >> ~~/benchmark-status ")

#;(define (gen-racket bench)
  (format racket-template bench bench bench bench))

#;(define (gen-script bench mode)
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

(define traces-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a ~a-warmup traces - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

PYPYLOG=jit-log-opt,jit-backend,jit-summary:../traces/~a-~a.trace ~a ~a/~a.rkt &>> ../timings-traces/~a-~a-~a-traces.rst

echo \"DONE ===> ~a ~a ~a ~a-warmup traces - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

(define with-warmup-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

~a ~a/~a.rkt &>> ../timings-pycket/~a-~a-~a-with-warmup.rst

echo \"DONE ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

(define racket-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

~a ~a/~a.rkt &>> ../timings-racket/~a-~a-~a.rst

echo \"DONE ===> ~a ~a ~a - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

(define without-warmup-template
  "#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd \"$PBS_O_WORKDIR\"
echo \"START ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status

for i in `seq 1 100`;
do
	~a ~a/~a.rkt &>> ../timings-pycket/~a-~a-~a-no-warmup.rst
done

echo \"DONE ===> ~a ~a ~a ~a-warmup - `date '+%Y-%m-%d %H:%M:%S'`\" >> ../experiment-status "
)

#|


|#

(module+ main
  (require racket/cmdline)

  (define sys #f) ;'(racket pycket))
  (define m #f) ;'(old new))
  (define warm #f) ;'(-with-warmup -no-warmup)) ; this is only for pycket

  (define traces #f)

  (define master-script-name "run-all.sh")
  (define racket-master-script "run-rackets.sh")

  (command-line
   #:once-any
   [("--racket") "generate racket scripts" (set! sys 'racket)]
   [("--pycket") "generate pycket scripts" (set! sys 'pycket)]
   #;[("--run-all-script") "generate the run-all.sh script for the sh files in the directory" (set! sys 'runall-script)]
   #:once-any
   [("--old") "old" (set! m 'old)]
   [("--new") "new" (set! m 'new)]
   #:once-any
   [("--with-warmup") "with warmup (for pycket)" (set! warm #t)]
   [("--no-warmup") "without warmup (for pycket)" (set! warm #f)]
   [("--traces") "with warmup, extract the JIT log" (set! warm #t) (set! traces #t)]

   #:args ()
   (printf "system : ~a - mode : ~a - warm : ~a (will have no effect if system is racket)\n"
           sys m warm)

   (when traces
         (unless (equal? sys 'pycket)
	 	 (error 'gen-scripts "system has to be pycket to generate JIT backend logs")))

   (when (equal? sys 'racket) (set! warm #t))

   (for ([b (in-list benchmarks)])
     (let* ([bash-script-name (if (equal? sys 'racket)
     	   		      	  (format "~a-~a-~a.sh" m sys b)
				  (if traces
				      (format "~a-~a-~a-traces.sh" m sys b)
     	   		      	      (format "~a-~a-~a-~a-warmup.sh" m sys b (if warm "with" "no"))))]
            [command (if (equal? sys 'racket)
                         (if (equal? m 'old) (format "~~/racketland/racket/bin/racket")
			     	       	     (format "~~/racketland/racket/racket/bin/racket"))
                         (if (equal? m 'new) "~/pycketland/pycket/pycket-c-linklets" "~/pycketland/pycket/pycket-c"))]
            [src-dir (if traces
	    	     	 "../src/for-traces"
	    	         (if warm 
                             "../src/with-warmup"
                             "../src/without-warmup"))])
       
       (call-with-output-file bash-script-name
         (lambda (bop)
           (display 
	   	    (if (equal? sys 'racket)
		    	(format racket-template
				m sys b
				command src-dir b
				m sys b
				m sys b)
			(if traces
			    (format traces-template
                            	    m sys b warm
			  	    m b
                            	    command src-dir b
                            	    m sys b
                            	    m sys b warm)
			    (format (if warm 
                                        with-warmup-template
				        without-warmup-template)
                                    m sys b warm
                            	    command src-dir b
                            	    m sys b
                            	    m sys b warm))) bop))
         #:exists 'replace)))

   (call-with-output-file master-script-name
     (lambda (op)
       (display "#!/bin/bash \n\n" op)
       (for ([p (directory-list)]) 
         (when (path-has-extension? p #".sh")
           (let ([p-str (path->string (file-name-from-path p))])
             (unless (string-contains? p-str "run-all")
               (displayln (format "qsub ~a" p) op))))))
     #:exists 'replace)

   (system "chmod 755 *.sh")
   ))
