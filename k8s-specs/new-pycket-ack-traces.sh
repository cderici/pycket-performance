#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd "$PBS_O_WORKDIR"
echo "START ===> new pycket ack #t-warmup traces - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status

PYPYLOG=jit-log-opt,jit-backend,jit-summary:../traces/new-ack.trace ~/pycketland/pycket/pycket-c-linklets ../src/for-traces/ack.rkt &>> ../timings-traces/new-pycket-ack-traces.rst

echo "DONE ===> new pycket ack #t-warmup traces - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status 