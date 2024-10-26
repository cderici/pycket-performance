all: all-racket-scripts all-pycket-scripts

RACKET_EXECUTABLE := $(shell which racket)
GENERATOR_SCRIPT := gen-kubejobs.rkt
SCRIPTS_DIR := scripts

.PHONY: all-pycket-scripts
all-pycket-scripts: all-pycket-old-scripts all-pycket-new-scripts

# Pycket NEW scripts

.PHONY: all-pycket-new-scripts
all-pycket-new-scripts: pycket-new-with-warmup-scripts pycket-new-no-warmup-scripts pycket-new-traces-scripts

.PHONY: pycket-new-with-warmup-scripts
pycket-new-with-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s --with-warmup

.PHONY: pycket-new-no-warmup-scripts
pycket-new-no-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s --no-warmup

.PHONY: pycket-new-traces-scripts
pycket-new-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -n -s -t


# Pycket OLD scripts

.PHONY: all-pycket-old-scripts
all-pycket-old-scripts: pycket-old-with-warmup-scripts pycket-old-no-warmup-scripts pycket-old-traces-scripts

.PHONY: pycket-old-with-warmup-scripts
pycket-old-with-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s --with-warmup

.PHONY: pycket-old-no-warmup-scripts
pycket-old-no-warmup-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -s --no-warmup

.PHONY: pycket-old-traces-scripts
pycket-old-traces-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -p -o -t -s

# Racket scripts

.PHONY: all-racket-scripts
all-racket-scripts:
	$(RACKET_EXECUTABLE) $(GENERATOR_SCRIPT) -r -s


# Utils

.PHONY: activate
activate:
	chmod 755 $(SCRIPTS_DIR)/*.sh

.PHONY: clean
clean: clean-racket clean-pycket clean-tmps
	rm -rf run-all.sh

.PHONY: clean-tmps
clean-tmps:
	rm -rf *.sh.o*
	rm -rf *~

.PHONY: clean-scripts
clean-scripts:
	rm -rf *.sh

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