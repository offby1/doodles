TESTS := at-private.ss at.ss ct2.ss misc-tests.ss

ETAGS ?= etags

tags: $(wildcard *.ss)
	$(ETAGS) --regex='/^ *(def\w+\W+.*?\W+/' *.ss
# Bill Clementson recommends this:
# --regex="/[ \t]*(def[a-z\-]+[ \t]+\(.*\)/\1/
# http://home.comcast.net/~bc19191/2003_11_02_bill-clementson_archive.html
# Haven't yet tried it

check: $(TESTS)
	@failed_tests=;						\
        for t in $(TESTS);					\
        do							\
                echo -n $$t ... '';				\
                ./$$t || failed_tests="$$failed_tests $$t";	\
        done; \
        test -z "$$failed_tests" || { echo Failed tests: $$failed_tests; false ; }

.PHONY: check tags
