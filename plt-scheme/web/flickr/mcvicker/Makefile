svnversion:= $(shell svnversion -n .)
basename  := $(shell echo flickr-thingy-$(svnversion) | sed s,:,-,)

# BUGBUG -- name it ".exe" only if we're on Windows.
exe = flickr-thingy.exe
pdir = flickr-thingy
zip = $(basename).zip

all: $(zip)

# All this complexity preserves the timestamp on version.ss if its
# contents haven't changed -- that way stuff that depends on it won't
# needlessly rebuild whenever we rebuild _it_.
version.ss: FORCE
	@temp=`mktemp botXXXXXX` ; \
	echo -n '(define *svnversion-string* "' > $${temp}; \
	echo -n $(svnversion) >> $${temp}; \
	echo    '")' >> $${temp}; \
	if ! cmp --quiet $${temp} $@; then mv -v $${temp} $@; fi; \
	rm -f $${temp}

FORCE:

# BUGBUG -- this works for Windows, but *nix puts it in
# $(pdir)/bin/$(exe).  "uname" might return "Linux", "CYGWIN_NT-5.1",
# "Darwin", or who knows what all else.
$(zip): $(pdir)/$(exe)
	zip -ur $@ $(pdir)

$(exe): $(wildcard *.ss) version.ss
	mzc --gui-exe $@ gui.ss 

$(pdir)/$(exe): $(exe)
	mzc --exe-dir $(pdir) $^

check: $(exe)
	PATH=.:$$PATH $^

.PHONY: check

.PHONY: clean
clean:
	rm -rf $(exe) $(zip) $(pdir) version.ss
