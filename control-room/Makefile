all: racket pycket

RACKET_EXECUTABLE := $(shell which racket)
GENERATOR_SCRIPT := gen-kubejobs.rkt
SCRIPTS_DIR := scripts

# run:
# 	./run-all.sh

pycket-new-traces:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) --pycket --new --traces

pycket-old-traces:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) --pycket --old --traces

racket:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) --racket --new

all-pycket: all-pycket-old all-pycket-new



all-pycket-old: pycket-old-with-warmup pycket-old-no-warmup

pycket-old-with-warmup:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) --pycket --old --with-warmup

pycket-old-no-warmup:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) --pycket --old --no-warmup



all-pycket-new: pycket-new-with-warmup pycket-new-no-warmup

pycket-new-with-warmup:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) --pycket --new --with-warmup

pycket-new-no-warmup:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) --pycket --new --no-warmup


activate:
	chmod 755 $(SCRIPTS_DIR)/*.sh

clean: clean-racket clean-pycket clean-tmps
	rm -rf run-all.sh

clean-tmps:
	rm -rf *.sh.o*
	rm -rf *~

clean-scripts:
	rm -rf *.sh

clean-racket:
	rm -rf $(SCRIPTS_DIR)/*-racket-*.sh

clean-pycket:
	rm -rf $(SCRIPTS_DIR)/*-pycket-*.sh

clean-old:
	rm -rf $(SCRIPTS_DIR)/old-*.sh

clean-new:
	rm -rf $(SCRIPTS_DIR)/new-*.sh

clean-with-warmup:
	rm -rf $(SCRIPTS_DIR)/*with-warmup*

clean-no-warmup:
	rm -rf $(SCRIPTS_DIR)/*no-warmup*