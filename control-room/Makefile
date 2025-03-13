

RACKET_EXECUTABLE := $(shell which racket)
GENERATOR_SCRIPT := gen-kubejobs.rkt
SCRIPTS_DIR := scripts
JOBS_DIR := jobs
DOCKER_IMAGE := canerderici/pycket-benchmark:latest


NFS_SHARE := /mnt/nfs_share
BENCHMARK_DIR := $(NFS_SHARE)/benchmarks
SRC_TARGET_DIR	  := $(BENCHMARK_DIR)/scripts
JOBS_TARGET_DIR  := $(BENCHMARK_DIR)/jobs

#################################################################
####################### TOP CONTROLS ############################
#################################################################

.PHONY: gen-all
gen-all: all-racket all-pycket

.PHONY: all-pycket
all-pycket: all-pycket-scripts all-pycket-jobs
	@echo "\nReady To Go\nConsider running 'make stage-all'"

.PHONY: all-pycket-trace
all-pycket-trace: all-pycket-trace-scripts all-pycket-trace-jobs
	@echo "\nReady To Go\nConsider running 'make stage-all'"

.PHONY: all-racket
all-racket: racket-scripts racket-jobs

.PHONY: stage-all
stage-all: stage-scripts stage-jobs

.PHONY: stage-scripts
stage-scripts:
	mv $(SCRIPTS_DIR)/*.sh $(SRC_TARGET_DIR)

.PHONY: stage-jobs
stage-jobs:
	mv $(JOBS_DIR)/*.yaml $(JOBS_TARGET_DIR)

#################################################################
########################## SCRIPTS ##############################
#################################################################

.PHONY: all-pycket-scripts
all-pycket-scripts: all-new-pycket-scripts all-old-pycket-scripts

.PHONY: all-pycket-trace-scripts
all-pycket-trace-scripts: all-new-pycket-trace-scripts all-old-pycket-trace-scripts

# Pycket NEW scripts

.PHONY: all-new-pycket-scripts
all-new-pycket-scripts: pycket-new-with-warmup-scripts pycket-new-no-warmup-scripts

.PHONY: all-new-pycket-trace-scripts
all-new-pycket-trace-scripts: pycket-new-with-warmup-traces-scripts pycket-new-no-warmup-traces-scripts

.PHONY: pycket-new-with-warmup-scripts
pycket-new-with-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s --with-warmup -l $(LABEL) noimage $(BENCHS)

.PHONY: pycket-new-no-warmup-scripts
pycket-new-no-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s --no-warmup -l $(LABEL) noimage $(BENCHS)

.PHONY: pycket-new-with-warmup-traces-scripts
pycket-new-with-warmup-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s -t --with-warmup -l $(LABEL) noimage $(BENCHS)

.PHONY: pycket-new-no-warmup-traces-scripts
pycket-new-no-warmup-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s -t --no-warmup -l $(LABEL) noimage $(BENCHS)


# Pycket OLD scripts

.PHONY: all-old-pycket-scripts
all-old-pycket-scripts: pycket-old-with-warmup-scripts pycket-old-no-warmup-scripts

.PHONY: all-old-pycket-trace-scripts
all-old-pycket-trace-scripts: pycket-old-with-warmup-traces-scripts pycket-old-no-warmup-traces-scripts

.PHONY: pycket-old-with-warmup-scripts
pycket-old-with-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s --with-warmup -l $(LABEL) noimage $(BENCHS)

.PHONY: pycket-old-no-warmup-scripts
pycket-old-no-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s --no-warmup -l $(LABEL) noimage $(BENCHS)

.PHONY: pycket-old-with-warmup-traces-scripts
pycket-old-with-warmup-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s -t --with-warmup -l $(LABEL) noimage $(BENCHS)

.PHONY: pycket-old-no-warmup-traces-scripts
pycket-old-no-warmup-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s -t --no-warmup -l $(LABEL) noimage $(BENCHS)


# Racket scripts

.PHONY: racket-scripts
racket-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -r -s -l $(LABEL) noimage $(BENCHS)

#################################################################
####################### KUBERNETES JOBS #########################
#################################################################

.PHONY: all-pycket-jobs
all-pycket-jobs: all-new-pycket-jobs all-old-pycket-jobs

.PHONY: all-pycket-trace-jobs
all-pycket-trace-jobs: all-new-pycket-trace-jobs all-old-pycket-trace-jobs

# Pycket NEW jobs

.PHONY: all-new-pycket-jobs
all-new-pycket-jobs: pycket-new-with-warmup-jobs pycket-new-no-warmup-jobs

.PHONY: all-new-pycket-trace-jobs
all-new-pycket-trace-jobs: pycket-new-with-warmup-traces-jobs pycket-new-no-warmup-traces-jobs


.PHONY: pycket-new-with-warmup-jobs
pycket-new-with-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -j --with-warmup $(DOCKER_IMAGE) $(BENCHS)

.PHONY: pycket-new-no-warmup-jobs
pycket-new-no-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -j --no-warmup $(DOCKER_IMAGE) $(BENCHS)

.PHONY: pycket-new-with-warmup-traces-jobs
pycket-new-with-warmup-traces-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -j -t --with-warmup $(DOCKER_IMAGE) $(BENCHS)

.PHONY: pycket-new-no-warmup-traces-jobs
pycket-new-no-warmup-traces-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -j -t --no-warmup $(DOCKER_IMAGE) $(BENCHS)

# Pycket OLD jobs

.PHONY: all-old-pycket-jobs
all-old-pycket-jobs: pycket-old-with-warmup-jobs pycket-old-no-warmup-jobs

.PHONY: all-old-pycket-trace-jobs
all-old-pycket-trace-jobs: pycket-old-with-warmup-traces-jobs pycket-old-no-warmup-traces-jobs

.PHONY: pycket-old-with-warmup-jobs
pycket-old-with-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -j --with-warmup $(DOCKER_IMAGE) $(BENCHS)

.PHONY: pycket-old-no-warmup-jobs
pycket-old-no-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -j --no-warmup $(DOCKER_IMAGE) $(BENCHS)

.PHONY: pycket-old-with-warmup-traces-jobs
pycket-old-with-warmup-traces-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -j -t --with-warmup $(DOCKER_IMAGE) $(BENCHS)

.PHONY: pycket-old-no-warmup-traces-jobs
pycket-old-no-warmup-traces-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -j -t --no-warmup $(DOCKER_IMAGE) $(BENCHS)

# Racket jobs

.PHONY: racket-jobs
racket-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -r -j $(DOCKER_IMAGE) $(BENCHS)

# Utils

.PHONY: clean-benchmark-site
clean-benchmark-site: clean-benchmark-site-scripts clean-benchmark-site-jobs

.PHONY: clean-benchmark-site-scripts
clean-benchmark-site-scripts:
	rm -rf $(SRC_TARGET_DIR)/*.sh

.PHONY: clean-benchmark-site-jobs
clean-benchmark-site-jobs:
	rm -rf $(JOBS_TARGET_DIR)/*.yaml

.PHONY: activate
activate:
	chmod 755 $(SCRIPTS_DIR)/*.sh

.PHONY: clear
clear: clean

.PHONY: clean
clean: clean-scripts clean-jobs

.PHONY: clean-scripts
clean-scripts:
	rm -rf $(SCRIPTS_DIR)/*.sh

.PHONY: clean-jobs
clean-jobs:
	rm -rf $(JOBS_DIR)/*.yaml

.PHONY: clean-racket
clean-racket:
	rm -rf $(SCRIPTS_DIR)/*racket-*.sh

.PHONY: clean-pycket
clean-pycket:
	rm -rf $(SCRIPTS_DIR)/*-pycket-*.sh

.PHONY: clean-old
clean-old:
	rm -rf $(SCRIPTS_DIR)/old-*.sh

.PHONY: clean-new
clean-new:
	rm -rf $(SCRIPTS_DIR)/new-*.sh

.PHONY: clean-with-warmup
clean-with-warmup:
	rm -rf $(SCRIPTS_DIR)/*with-warmup*

.PHONY: clean-no-warmup
clean-no-warmup:
	rm -rf $(SCRIPTS_DIR)/*no-warmup*

.PHONY: clean-tmps
clean-tmps:
	rm -rf *.sh.o*
	rm -rf *~
