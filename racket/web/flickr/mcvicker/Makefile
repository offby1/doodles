svnversion:= $(shell svnversion -n .)
basename  := $(shell echo flickr-thingy-$(svnversion) | sed s,:,-,)

# BUGBUG -- name it ".exe" only if we're on Windows.
exe = flickr-thingy.exe
all: $(exe)

# All this complexity preserves the timestamp on version.ss if its
# contents haven't changed -- that way stuff that depends on it won't
# needlessly rebuild whenever we rebuild _it_.
version.ss: FORCE
	@temp=`mktemp botXXXXXX` ; \
	/bin/echo -n '(define *svnversion-string* "' > $${temp}; \
	/bin/echo -n $(svnversion) >> $${temp}; \
	/bin/echo    '")' >> $${temp}; \
	if ! cmp --quiet $${temp} $@; then mv -v $${temp} $@; fi; \
	rm -f $${temp}

FORCE:

$(exe): $(wildcard *.ss) version.ss
	raco exe --gui gui.ss 

check: $(exe)
	PATH=.:$$PATH $^

.PHONY: check

.PHONY: clean
clean:
	rm -rf $(exe) $ version.ss
