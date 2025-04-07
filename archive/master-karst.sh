#!/bin/bash
#PBS -k o
#PBS -l nodes=1:ppn=16,walltime=24:00:00
#PBS -M canerderici+karst@gmail.com
#PBS -m a
#PBS -j oe
  
cd "$PBS_O_WORKDIR"

echo "Subject: master-karst.sh is done."

cd ./control-room
make clean
echo > ../experiment-status
echo "cleaning done."
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
echo "scripts are generated, starting to run ..."
make run
# wait for the new* jobs to finish
while true; do
    wait=0
    while read -r line; do
	if [[ $line == *"new-"* ]]; then
	    wait=1
	    break
	fi
    done < <(qstat -u cderici)
    if [[ $wait -eq 0 ]]; then
	echo "new-* jobs are done, start the old-* tests"
	break
    else
	echo "waiting... `date '+%Y-%m-%d %H:%M:%S'`"
	sleep 10m
    fi
done

make clean-scripts
if [ "${WHAT}" = "traces" ]; then
    make old-traces
else
    make pycket-old
fi
echo "scripts are generated, starting to run"
make run
# wait for the new* jobs to finish
while true; do
    wait=0
    while read -r line; do
	if [[ $line == *"old-"* ]]; then
	    wait=1
	    break
	fi
    done < <(qstat -u cderici)
    if [[ $wait -eq 0 ]]; then
	echo "old-* jobs are done, start the analysis"
	break
    else
	echo "waiting... `date '+%Y-%m-%d %H:%M:%S'`"
	sleep 10m
    fi
done
echo "cleaning and exiting"
make clean-scripts
cd ..
echo "analysis starts"
racket analyze-rebench-output.rkt ${WHAT}
cp experiment-status tmp/experiment-status_`date '+%Y-%m-%d_%H:%M:%S'`
echo "sending email report"
sendmail canerderici@gmail.com < master-karst.sh.*