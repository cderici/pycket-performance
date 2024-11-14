
PYTHON := /usr/bin/python3

.PHONY: all
all: all-non-relative all-relative all-no-warmup

# Non-relative
.PHPNY: all-non-relative
all-non-relative: new-vs-old new-vs-racket old-vs-racket new-vs-old-vs-racket

.PHONY: new-vs-old
new-vs-old:
	$(PYTHON) main.py $(DIR) --new --old --with-warmup

.PHONY: new-vs-racket
new-vs-racket:
	$(PYTHON) main.py $(DIR) --new --racket --with-warmup

.PHONY: old-vs-racket
old-vs-racket:
	$(PYTHON) main.py $(DIR) --old --racket --with-warmup

.PHONY: new-vs-old-vs-racket
new-vs-old-vs-racket:
	$(PYTHON) main.py $(DIR) --new --old --racket --with-warmup

# Relative
.PHONY: all-relative
all-relative: new-vs-racket-relative-racket new-vs-old-relative-racket

.PHONY: new-vs-racket-relative-racket
new-vs-racket-relative-racket:
	$(PYTHON) main.py $(DIR) --new --racket --with-warmup --relative racket

.PHONY: new-vs-old-relative-racket
new-vs-old-relative-racket:
	$(PYTHON) main.py $(DIR) --new --old --racket --with-warmup --relative racket

# No Warmup

.PHONY: all-no-warmup
all-no-warmup: new-vs-old-no-warmup new-vs-old-vs-racket-no-warmup

.PHONY: new-vs-old-no-warmup
new-vs-old-no-warmup:
	$(PYTHON) main.py $(DIR) --new --old --no-warmup

.PHONY: new-vs-old-vs-racket-no-warmup
new-vs-old-vs-racket-no-warmup:
	$(PYTHON) main.py $(DIR) --new --old --racket --no-warmup

.PHONY: clean
clean:
	rm -rf *.png

.PHONY: all-singles
all-singles:
	$(PYTHON) main.py $(DIR) --single all