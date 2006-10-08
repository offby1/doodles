# -*-makefile-*-
check:
	@echo Seeing octal escapes in Emacs?  Try C-x RET c utf-8 RET M-x make
	@echo "And use Lucida Console, if your terminal is running on Windows;"
	@echo "on *nix, '10x20' from Emacs' font menu also works"
	./tests.ss
	./deck.ss --profile --lookahead 0 --quiet

dds: $(wildcard *.ss)
	mzc --exe $@ deck.ss

ETAGS ?= etags

tags: TAGS

TAGS: $(wildcard *.ss)
	$(ETAGS) --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

clean:
	rm -rf dds TAGS coverage

.PHONY: check clean tags
