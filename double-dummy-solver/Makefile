# -*-makefile-*-
TESTS = tests test-predictions fill-out-hands filler

check: compile
	./tests.ss
	./test-predictions.ss --lookahead 0 --quiet
	./fill-out-hands.ss
	./filler.ss --lookahead 0 --seconds-per-card 1/2 --number-of-hands 1

profile: clean
	./profile.ss --quiet

compile:
	for i in $(wildcard *.ss); do mzc --make $$i.ss; done

deps: deps.pdf

deps.dot: compile
	./make-dep-graph.ss > $@

.INTERMEDIATE: deps.dot

%.ps : %.dot
	dot -Tps $^ > $@

%.pdf: %.ps
	ps2pdf $^

ETAGS ?= etags

tags: TAGS

TAGS: $(wildcard *.ss)
	$(ETAGS) --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

# On windows, you want
#	PATH=$$(cygpath $$EMACSPATH):$$PATH $(ETAGS) ...

clean:
	rm -rf TAGS coverage compiled *.pdf

superclean:
	non-versioned-files.pl -0 | xargs -0 --no-run-if-empty rm -rfv

.PHONY: check clean compile deps superclean tags
