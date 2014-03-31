
all: Lambda

Lambda: *.hs
#	ghc --make -prof -fprof-auto -rtsopts Lambda
	ghc -O3 --make Lambda

test:
	perl test.t

clean:
	rm -f Lambda *.o *.hi *~


.PHONY: all clean
