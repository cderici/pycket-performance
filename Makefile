karst:
	qsub -v WHAT="timings" master-karst.sh

traces:
	qsub -v WHAT="traces" master-karst.sh
