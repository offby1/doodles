TESTS := at-private.ss at.ss ct2.ss make-call.ss misc-tests.ss tree-test.ss

ETAGS ?= etags

# Some comments about the complex regular expression:

#I think it's not necessary for this regular expression to carefully
#pick out the name of the defined thing, because etags can guess the
#name.  But it doesn't seem to hurt to have it, and boy did I slave
#over it ...

#Also, for some reason, tags completion doesn't work with structure
#names, but works fine with function names.

tags: $(wildcard *.ss)
	$(ETAGS) --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

check: $(TESTS)
	@failed_tests=;						\
        for t in $(TESTS);					\
        do							\
                echo -n $$t ... '';				\
                ./$$t || failed_tests="$$failed_tests $$t";	\
        done; \
        test -z "$$failed_tests" || { echo Failed tests: $$failed_tests; false ; }
	./auction.ss
	./call.ss
	./tree.ss
	mzscheme -qu play-card.ss
	mzscheme -qu deck-fiddling.ss

.PHONY: check tags
