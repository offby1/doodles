srcs := anagrams.scm dict.scm bag.scm
objs := $(srcs:.scm=.o)
cs   := $(srcs:.scm=.c)

CFLAGS := $(shell chicken-config -cflags)
LDFLAGS:= $(shell chicken-config -libs)

%.c : %.scm
	chicken $^

%.o : %.c
	gcc -c $^ $(CFLAGS)

anagrams: $(objs)
	gcc $^ -o $@ $(LDFLAGS)

words: /usr/share/dict/words
	ln -sv $^ $@

check: anagrams words
	time ./$^

clean:
	rm -f $(objs) $(cs) anagrams

.PHONY: clean check
