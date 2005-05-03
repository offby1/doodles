SCMS 	 := bag.scm dict.scm hash-table.scm anagrams.scm 
CS   	 := $(SCMS:.scm=.c)
LINKFILE := $(basename $(word $(words $(SCMS)),$(SCMS)))_.c

prog := anagrams

all: $(prog)

check: $(prog)
	time ./$(prog) "st hemingway"

%.c : %.scm
	gsc -c $^

$(LINKFILE): $(CS)
	gsc $^

LDLIBS := -lgambc -lm -lutil -ldl 

$(prog): $(CS) $(LINKFILE)

clean:
	rm -f $(CS) $(LINKFILE) $(prog) hmm

.PHONY: clean check
