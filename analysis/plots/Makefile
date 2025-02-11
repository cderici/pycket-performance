
PYTHON := /usr/bin/python3

.PHONY: all
all: all-non-relative all-relative all-no-warmup # all-singles

# Non-relative
.PHONY: all-non-relative
all-non-relative: new-vs-old # new-vs-old-vs-racket # new-vs-racket

.PHONY: new-vs-old
new-vs-old:
	$(PYTHON) main.py $(DIR) --new --old --with-warmup --label $(LABEL)

# .PHONY: new-vs-racket
# new-vs-racket:
# 	$(PYTHON) main.py $(DIR) --new --racket --with-warmup

# .PHONY: new-vs-old-vs-racket
# new-vs-old-vs-racket:
# 	$(PYTHON) main.py $(DIR) --new --old --racket --with-warmup

# Relative
.PHONY: all-relative
all-relative: new-vs-racket-relative-racket new-vs-old-relative-racket old-vs-racket-relative-racket

.PHONY: new-vs-racket-relative-racket
new-vs-racket-relative-racket:
	$(PYTHON) main.py $(DIR) --new --racket --with-warmup --relative racket --label $(LABEL)

.PHONY: new-vs-old-relative-racket
new-vs-old-relative-racket:
	$(PYTHON) main.py $(DIR) --new --old --racket --with-warmup --relative racket --label $(LABEL)

.PHONY: old-vs-racket-relative-racket
old-vs-racket-relative-racket:
	$(PYTHON) main.py $(DIR) --old --racket --with-warmup --relative racket --label $(LABEL)

# No Warmup

.PHONY: all-no-warmup
all-no-warmup: new-vs-old-no-warmup new-vs-old-vs-racket-no-warmup

.PHONY: new-vs-old-no-warmup
new-vs-old-no-warmup:
	$(PYTHON) main.py $(DIR) --new --old --no-warmup --label $(LABEL)

.PHONY: new-vs-old-vs-racket-no-warmup
new-vs-old-vs-racket-no-warmup:
	$(PYTHON) main.py $(DIR) --new --old --racket --no-warmup --relative racket --label $(LABEL)

.PHONY: clean
clean:
	rm -rf *.png

.PHONY: all-singles
all-singles:
	$(PYTHON) main.py $(DIR) --new --old --racket --single all --relative racket --label $(LABEL)


