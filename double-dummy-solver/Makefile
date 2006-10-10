# -*-makefile-*-
check: dds
	./tests.ss
	./dds --lookahead 0 

profile:
	./profile.ss --quiet

dds: $(wildcard *.ss)
	mzc --exe $@ deck.ss

ETAGS ?= etags

tags: TAGS

TAGS: $(wildcard *.ss)
	$(ETAGS) --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

clean:
	rm -rf dds TAGS coverage

.PHONY: check clean tags
