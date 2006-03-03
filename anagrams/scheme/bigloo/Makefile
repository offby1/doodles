anagrams: $(addsuffix .o,anagrams bag dict)

%.o : %.scm
	bigloo -g4 -c $^

% : %.o
	bigloo -o $@ $^

check: anagrams
	time ./$^ "Hemingway" > /dev/null 2> /tmp/bad

clean:
	rm -f anagrams *.o

.PHONY: check clean
