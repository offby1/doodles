SCMS 	 := bag.scm dict.scm anagrams.scm 
CS   	 := $(SCMS:.scm=.c)
LINKFILE := $(basename $(word $(words $(SCMS)),$(SCMS)))_.c

prog := anagrams

all: $(prog)

check: $(prog)
	rm -f crapola
	for i in "hemingway" "st hemingway" "est hemingway"; \
	do echo $$i; time ./$(prog) $$i >> crapola; done

%.c : %.scm
	gsc -c $^

$(LINKFILE): $(CS)
	gsc $^

LDLIBS := -lgambc -lm -lutil -ldl 

$(prog): $(CS) $(LINKFILE)

clean:
	rm -f $(CS) $(LINKFILE) $(prog) crapola hmm *.o

.PHONY: clean check
.INTERMEDIATE: $(CS) $(LINKFILE)
