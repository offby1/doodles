PROG := anagrams
PROF := $(PROG).prof

prof: $(PROF)

$(PROF): $(PROG)
	time ./$^ +RTS -p

check: $(PROG)
	time ./$^

$(PROG): $(wildcard *.hs)
	ghc -O -prof -auto-all --make -o $@ $@.hs

clean:
	-rm *.o *.hi $(PROG) $(PROF)

.PHONY: clean check prof
