#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd "$PBS_O_WORKDIR"

cd ./control-room
make clean
echo > ../experiment-status
if [ "${WHAT}" = "traces" ]; then
    rm -rf ../timings-traces
    mkdir ../timings-traces
    rm -rf ../traces # fixme: make these conditional and parameterize (don't automatically delete things)
    mkdir ../traces
    make new-traces
else
    rm -rf ../timings-pycket
    mkdir ../timings-pycket
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
racket analyze-rebench-output.rkt ${WHAT}
cp experiment-status tmp/experiment-status_`date '+%Y-%m-%d_%H:%M:%S'`
