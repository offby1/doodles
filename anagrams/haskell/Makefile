PROG := anagrams
PROF := $(PROG).prof

all: $(PROG)

prof: $(PROF)

$(PROF): $(PROG)
	time ./$^ ernest +RTS -p  > /dev/null

check: $(PROG)
	time ./$^ ernest > /dev/null
	@if [ -e $(PROF) -a ! -s $(PROF) ]; then echo Removing empty $(PROF); rm $(PROF); fi

$(PROG): $(wildcard *.hs)
	ghc -O -prof -auto-all --make -o $@ $@.hs

clean:
	-rm *.o *.hi $(PROG) $(PROF)

.PHONY: clean check prof all
