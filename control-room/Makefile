
all: racket pycket

run:
	./run-all.sh

new-traces:
	racket gen-scripts.rkt --pycket --new --traces

old-traces:
	racket gen-scripts.rkt --pycket --old --traces

racket: racket-old racket-new

racket-old: 
	racket gen-scripts.rkt --racket --old

racket-new:
	racket gen-scripts.rkt --racket --new

pycket: pycket-old pycket-new

pycket-old: pycket-old-with-warmup


 # pycket-old-no-warmup		

pycket-old-with-warmup:
	racket gen-scripts.rkt --pycket --old --with-warmup

# pycket-old-no-warmup:
# 	racket gen-scripts.rkt --pycket --old --no-warmup

pycket-new: pycket-new-with-warmup 

# pycket-new-no-warmup

pycket-new-with-warmup:
	racket gen-scripts.rkt --pycket --new --with-warmup

# pycket-new-no-warmup:
# 	racket gen-scripts.rkt --pycket --new --no-warmup


activate:
	chmod 755 *.sh

clean: clean-racket clean-pycket clean-tmps
	rm -rf run-all.sh

clean-tmps:
	rm -rf *.sh.o*
	rm -rf *~

clean-scripts:
	rm -rf *.sh

clean-racket:
	rm -rf *-racket-*.sh

clean-pycket:
	rm -rf *-pycket-*.sh

clean-old:
	rm -rf old-*.sh

clean-new:
	rm -rf new-*.sh

clean-with-warmup:
	rm -rf *with-warmup*

clean-no-warmup:
	rm -rf *no-warmup*