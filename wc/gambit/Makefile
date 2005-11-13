SCMS 	 := forcer.ss filter.ss
CS   	 := $(SCMS:.ss=.c)
LINKFILE := $(basename $(word $(words $(SCMS)),$(SCMS)))_.c

prog := forcer

all: $(prog)

check: $(prog)
	time ./$(prog) /usr/share/dict/words giant raven

%.c : %.ss
	gsc -c $^

$(LINKFILE): $(CS)
	gsc $^

LDLIBS := -lgambc -lm -lutil -ldl 

$(prog): $(CS) $(LINKFILE)

clean:
	rm -f $(CS) $(LINKFILE) $(prog) hmm *.o

.PHONY: clean check
.INTERMEDIATE: $(CS) $(LINKFILE)
