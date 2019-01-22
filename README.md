# pycket-performance

This repository contains the setup for running (some of) the cross-benchmarks on Pycket and get the timing results ready to be analysed. Note that it's currently designed to be run on a cluster machine with PBS.

Cloning the repo and typing `make` will invoke the master script which interacts with the `control-room` to run everything. Here's what's gonna happen in order:

- clean the `control-room`
- clean the `timings-pycket`
- clean the `experiment-status`
- generate `pycket-new` scripts (for each benchmark)
- invoke the generated `run-all.sh` which will `qsub` all the scripts
- wait 30 minutes [*]
- clean the scripts in `control-room` (the job log files will be kept)
- generate `pycket-old` scripts
- invoke the (newly) generated `run-all.sh` which will `qsub` all the old-pycket scripts
- wait 30 minutes [*]
- clean the scripts in `control-room` (the job log files are still there)
- invoke `analyze-rebench-output.rkt` to pull all the timings together into one output file.
- save the `experiment-status` into `tmp` with a timestamp of the experiment

The output file is going to be `pyckets.data`. It's currently formatted to be recognized by a particular R script, which is a modified version of the one that's in [Pycket-Bench][https://github.com/pycket/pycket-bench].

The `pyckets.data` file can be used to generate a final pdf containing the result graph using the R script in the `tmp` folder. 

> ./tmp/pyckets_analyze_benchmarks.R pyckets.data

The entire operation takes roughly an hour.

Typing `make traces` will perform a similar operation, only with lesser iteration times, so it will take shorter, and it'll output the trace log files for each benchmark into `traces` folder.

[*] It usually takes 15-20mins to run the entire suite (in separate nodes of course), but the script allocates 30mins just in case of delays caused by cluster scheduling.
