anagrams: $(addsuffix .o,anagrams bag dict)

%.o : %.scm
	bigloo -g4 -c $^

% : %.o
	bigloo -o $@ $^

check: anagrams
	time ./$^ "Ernest Hemingway" > /dev/null

clean:
	rm -f anagrams *.o

.PHONY: check clean
