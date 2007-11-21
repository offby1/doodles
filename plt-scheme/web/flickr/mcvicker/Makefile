svnversion:= $(shell svnversion -n .)
basename  := $(shell echo flickr-thingy-$(svnversion) | sed s,:,-,)

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

$(zip): $(pdir)/$(exe)
	zip -ur $@ $(pdir)

$(exe): $(wildcard *.ss)
	mzc --gui-exe $@ gui.ss 

$(pdir)/$(exe): $(exe)
	mzc --exe-dir $(pdir) $^

check: $(exe)
	PATH=.:$$PATH $^

.PHONY: check
