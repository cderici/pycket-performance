#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd "$PBS_O_WORKDIR"
echo "START ===> old pycket ack #f-warmup - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status

for i in `seq 1 100`;
do
	~/pycketland/pycket/pycket-c ../src/without-warmup/ack.rkt &>> ../timings-pycket/old-pycket-ack-no-warmup.rst
done

echo "DONE ===> old pycket ack #f-warmup - `date '+%Y-%m-%d %H:%M:%S'`" >> ../experiment-status 