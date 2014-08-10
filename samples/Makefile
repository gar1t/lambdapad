samples = 

gen: $(patsubst %/Makefile,_GEN_%,$(wildcard */Makefile))

clean: $(patsubst %/Makefile,_CLEAN_%,$(wildcard */Makefile))

_GEN_%:
	cd $* && make gen

_CLEAN_%:
	cd $* && make clean

