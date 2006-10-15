# -*-makefile-*-
check: dds
	./tests.ss
	./dds --lookahead 0 --quiet
	./fill-out-hands.ss

profile:
	./profile.ss --quiet

dds: $(wildcard *.ss)
	mzc --exe $@ test-predictions.ss

ETAGS ?= etags

tags: TAGS

TAGS: $(wildcard *.ss)
	$(ETAGS) --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

clean:
	rm -rf dds TAGS coverage

.PHONY: check clean tags
