anagrams: $(addsuffix .o,anagrams bag dict)

# -g enables assertions
%.o : %.scm
	bigloo -g -g4 -c $^

% : %.o
	bigloo -g4 -o $@ $^

check: anagrams
	time ./$^ "Ernest Hemingway" > /dev/null

clean:
	rm -f anagrams *.o

.PHONY: check clean
