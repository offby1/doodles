PROG := anagrams

CLEANFILES := $(PROG)
PROF := $(PROG).prof
CLEANUP := @if [ -e $(PROF) -a ! -s $(PROF) ]; then echo Removing empty $(PROF); rm $(PROF); fi
CLEANFILES += $(PROF)

#profiling := t

ifdef profiling
GHCARGS :=  -prof -auto-all
prof: $(PROF)

$(PROF): $(PROG)
	time ./$^ ernest +RTS -p  > /dev/null

endif

all: $(PROG)

check: $(PROG)
	time ./$^ ernest > /dev/null
	$(CLEANUP)

$(PROG): $(wildcard *.hs)
	ghc -O $(GHCARGS) --make -o $@ $@.hs

clean:
	-rm *.o *.hi $(CLEANFILES)

.PHONY: clean check prof all
