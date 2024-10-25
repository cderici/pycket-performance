#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd "$PBS_O_WORKDIR"
echo "START ===> new racket ack - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status

~/racketland/racket/racket/bin/racket ../src/with-warmup/ack.rkt &>> ../timings-racket/new-racket-ack.rst

echo "DONE ===> new racket ack - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status 