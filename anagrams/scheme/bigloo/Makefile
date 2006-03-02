anagrams: $(addsuffix .o,anagrams bag dict)

%.o : %.scm
	bigloo -c $^

% : %.o
	bigloo -o $@ $^
