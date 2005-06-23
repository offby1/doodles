%.o : %.scm
	bigloo -c $^

% : %.o
	bigloo -o $@ $^
