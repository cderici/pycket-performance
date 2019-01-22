#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd "$PBS_O_WORKDIR"

cd ./control-room
make clean
cd ..
rm -f timings-pycket/*
echo > experiment-status
cd ./control-room
if [ "${WHAT}" = "traces" ]; then
    make new-traces
else
    make pycket-new
fi
make run
sleep 30m
make clean-scripts
if [ "${WHAT}" = "traces" ]; then
    make old-traces
else
    make pycket-old
fi
make run
sleep 30m
make clean-scripts
cd ..
racket analyze-rebench-output.rkt
cp experiment-status tmp/experiment-status_`date '+%Y-%m-%d_%H:%M:%S'`
