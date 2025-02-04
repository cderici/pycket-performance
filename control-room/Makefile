

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

.PHONY: all
all: gen-all stage-all

.PHONY: gen-all
gen-all: all-racket all-pycket

.PHONY: all-pycket
all-pycket: all-pycket-scripts all-pycket-jobs

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
all-pycket-scripts: all-pycket-new-scripts all-pycket-old-scripts

# Pycket NEW scripts

.PHONY: all-pycket-new-scripts
all-pycket-new-scripts: pycket-new-with-warmup-scripts pycket-new-no-warmup-scripts pycket-new-with-warmup-traces-scripts pycket-new-no-warmup-traces-scripts



.PHONY: pycket-new-with-warmup-scripts
pycket-new-with-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s --with-warmup

.PHONY: pycket-new-no-warmup-scripts
pycket-new-no-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s --no-warmup

.PHONY: pycket-new-with-warmup-traces-scripts
pycket-new-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s -t --with-warmup

.PHONY: pycket-new-no-warmup-traces-scripts
pycket-new-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s -t --no-warmup


# Pycket OLD scripts

.PHONY: all-pycket-old-scripts
all-pycket-old-scripts: pycket-old-with-warmup-scripts pycket-old-no-warmup-scripts pycket-old-with-warmup-traces-scripts pycket-old-no-warmup-traces-scripts



.PHONY: pycket-old-with-warmup-scripts
pycket-old-with-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s --with-warmup

.PHONY: pycket-old-no-warmup-scripts
pycket-old-no-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s --no-warmup

.PHONY: pycket-old-with-warmup-traces-scripts
pycket-old-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s -t --with-warmup

.PHONY: pycket-old-no-warmup-traces-scripts
pycket-old-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s -t --no-warmup


# Racket scripts

.PHONY: racket-scripts
racket-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -r -s

#################################################################
####################### KUBERNETES JOBS #########################
#################################################################

.PHONY: all-pycket-jobs
all-pycket-jobs: all-pycket-new-jobs all-pycket-old-jobs

# Pycket NEW jobs

.PHONY: all-pycket-new-jobs
all-pycket-new-jobs: pycket-new-with-warmup-jobs pycket-new-no-warmup-jobs pycket-new-traces-jobs

.PHONY: pycket-new-with-warmup-jobs
pycket-new-with-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -j --with-warmup $(DOCKER_IMAGE)

.PHONY: pycket-new-no-warmup-jobs
pycket-new-no-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -j --no-warmup $(DOCKER_IMAGE)

.PHONY: pycket-new-traces-jobs
pycket-new-traces-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -j -t $(DOCKER_IMAGE)

# Pycket OLD jobs

.PHONY: all-pycket-old-jobs
all-pycket-old-jobs: pycket-old-with-warmup-jobs pycket-old-no-warmup-jobs pycket-old-traces-jobs

.PHONY: pycket-old-with-warmup-jobs
pycket-old-with-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -j --with-warmup $(DOCKER_IMAGE)

.PHONY: pycket-old-no-warmup-jobs
pycket-old-no-warmup-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -j --no-warmup $(DOCKER_IMAGE)

.PHONY: pycket-old-traces-jobs
pycket-old-traces-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -t -j $(DOCKER_IMAGE)

# Racket jobs

.PHONY: racket-jobs
racket-jobs:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -r -j $(DOCKER_IMAGE)

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
