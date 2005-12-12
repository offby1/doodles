srcs := anagrams.scm dict.scm bag.scm
objs := $(srcs:.scm=.o)
cs   := $(srcs:.scm=.c)

CFLAGS := $(shell chicken-config -cflags)
LDFLAGS:= $(shell chicken-config -libs)

%.c : %.scm
	chicken $^ -profile

%.o : %.c
	gcc -c $^ $(CFLAGS)

anagrams: $(objs)
	gcc $^ -o $@ $(LDFLAGS)

words: /usr/share/dict/words
	ln -sv $^ $@

check: anagrams words
	time ./$^ && chicken-profile

clean:
	rm -f $(objs) $(cs) anagrams PROFILE

.PHONY: clean check
