#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd "$PBS_O_WORKDIR"
echo "START ===> old pycket ack #t-warmup - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status

~/pycketland/pycket/pycket-c ../src/with-warmup/ack.rkt &>> ../timings-pycket/old-pycket-ack-with-warmup.rst

echo "DONE ===> old pycket ack #t-warmup - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status 