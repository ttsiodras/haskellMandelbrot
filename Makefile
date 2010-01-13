.PHONY:	bench clean

TARGETS = mandel.first mandel.second mandel.third

all:	${TARGETS} bench

% : %.hs
	ghc -o $@ -W -fglasgow-exts -O --make $<

bench:	${TARGETS}
	@for i in $^ ; do echo Output of $${i}... ; ./$$i > data.pnm && display ./data.pnm ; echo Benchmarking 3 times... ; time ./$$i > /dev/null ;  time ./$$i > /dev/null ;  time ./$$i > /dev/null ; done

clean:
	rm -f *.o *.hi data.pnm ${TARGETS}
