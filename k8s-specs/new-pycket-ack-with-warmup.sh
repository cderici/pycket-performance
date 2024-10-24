#!/bin/bash

NFS_SHARE=/mnt/nfs_share
BENCH_DIR=$NFS_SHARE/benchmarks
PYCKET_DIR=/opt/pycket
SOURCE_DIR=$NFS_SHARE/pycket-performance/src
OUTPUT_DIR=$BENCH_DIR/timings-pycket


echo "new pycket ack #t-warmup - `date '+%Y-%m-%d %H:%M:%S'` - STARTED on pod: $POD_NAME" >> $BENCH_DIR/experiment-status

$PYCKET_DIR/pycket-c-linklets $SOURCE_DIR/with-warmup/ack.rkt &>> $OUTPUT_DIR/new-pycket-ack-with-warmup.rst

echo "new pycket ack #t-warmup - `date '+%Y-%m-%d %H:%M:%S'` - COMPLETED on pod: $POD_NAME" >> $BENCH_DIR/experiment-status