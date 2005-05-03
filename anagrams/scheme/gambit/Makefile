#  bag-example.scm
SCMS := bag.scm dict.scm hash-table.scm anagrams.scm 
CS   := $(SCMS:.scm=.c) $(basename $(word $(words $(SCMS)),$(SCMS)))_.c

prog := yow

all: $(prog)

check: $(prog)
	time ./$(prog) "st hemingway"

$(prog): $(SCMS)
	gsc $^
	gcc -o $@ $(CS) -lgambc -lm -lutil -ldl 

clean:
	rm -f *.c $(prog) hmm

.PHONY: clean check
