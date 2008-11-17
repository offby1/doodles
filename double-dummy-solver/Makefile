# -*-makefile-*-
TESTS = tests test-predictions fill-out-hands filler

check:
	mzscheme ./tests.ss
	@echo -e '\f'
	mzscheme ./test-predictions.ss --lookahead 0 --quiet
	@echo -e '\f'
	mzscheme ./fill-out-hands.ss
	@echo -e '\f'
	mzscheme ./filler.ss --lookahead 0 --seconds-per-card 1/2 --number-of-hands 1 --rng "#6(515534532 3624771765 617072659 3828512826 3152960333 3219854132)"
# There's nothing special about that particular --rng argument; I just
# thought it'd be nice to have more-or-less deterministic output from
# "make check"


profile: clean
	mzscheme ./profile.ss --seconds-per-card 1/10

compile:
	for i in $(filter-out profile.ss,$(wildcard *.ss)); do mzc --make $$i; done

deps: deps.pdf

deps.dot: compile
	mzscheme ./make-dep-graph.ss > $@

.INTERMEDIATE: deps.dot

%.ps : %.dot
	dot -Tps $^ > $@

%.pdf: %.ps
	ps2pdf $^

ETAGS ?= etags

tags: TAGS

TAGS: $(wildcard *.ss)
	PATH=/usr/local/src/emacs-cvs/lib-src:$$PATH:$(dir $(EMACS))../lib-src  $(ETAGS) --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

clean:
	rm -rf TAGS coverage compiled *.pdf

superclean:
	non-versioned-files.pl -0 | xargs -0 --no-run-if-empty rm -rfv

.PHONY: check clean compile deps superclean tags
