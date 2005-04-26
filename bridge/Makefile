TESTS := at-private.ss at.ss ct2.ss

check: $(TESTS)
	@failed_tests=;									\
        for t in $(TESTS);								\
        do										\
                echo -n $$t ... '';							\
		./$$t || failed_tests="$$failed_tests $$t";				\
        done;										\
        test -z "$$failed_tests" || { echo Failed tests: $$failed_tests; false ; }

.PHONY: check
