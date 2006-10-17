# -*-makefile-*-
check: compile
	./tests.ss
	./test-predictions.ss --lookahead 0 --quiet
	./fill-out-hands.ss

profile: clean
	./profile.ss --quiet

compile:
	for i in tests test-predictions fill-out-hands; do mzc --make $$i.ss; done

ETAGS ?= etags

tags: TAGS

TAGS: $(wildcard *.ss)
	$(ETAGS) --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

clean:
	rm -rf TAGS coverage compiled

.PHONY: check clean compile tags
