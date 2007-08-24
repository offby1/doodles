svnversion:= $(shell svnversion -n .)
basename  := $(shell echo rudybot-$(svnversion) | sed s,:,-,)
plt-file  := $(basename).plt
tarball   := $(basename).tar.gz
distdir   := $(HOME)/public_html/bot

all: plt tar
plt: $(plt-file)
tar: $(tarball)

define make-export-dir
tempdir=`mktemp -d botXXXXXX` ; \
expdir=$${tempdir}/$(basename) ; \
svn export -rbase . $${expdir}
endef

define cleanup
rm -rf $${tempdir}
endef

version.ss: $(filter-out version.ss,$(wildcard *.ss))
	echo -n '(define *svnversion-string* "' > $@
	echo -n $(svnversion) >> $@
	echo    '")' >> $@

$(plt-file): version.ss
	$(make-export-dir) && \
	cp $^ $${expdir} && \
	planet --create-archive $${expdir} && \
	$(cleanup)

$(tarball): version.ss
	$(make-export-dir) && \
	cp $^ $${expdir} && \
	tar -C $${tempdir} --create --gzip --file=$@ $(basename) && \
	$(cleanup)

svnup: 
	svn up

dist: svnup $(tarball)
	@mv -v $(tarball) $(distdir)
	@ln -svf $(distdir)/$(tarball) $(distdir)/rudybot.tar.gz

# To be blunt, run-all-tests.ss doesn't test as much as it should;
# dry-test hits some spots that run-all-tests misses.  So we run
# dry-tests too.  Alas, it doesn't report failures; you have to
# carefully examine the output for runtime errors.
check:
	./run-all-tests.ss
	@echo Feel free to kill the following if it runs for too long.
	./dry-test.sh

clean:
	rm -rf $(plt-file) $(tarball)

tags: TAGS

# This tags file is almost useless -- it just contains the names of
# the scheme files, but doesn't contain any actual tags.  But it's
# useful for emacs commands that work on all the files in it, such as
# tags-query-replace.
TAGS: $(wildcard *.ss)
	PATH=/usr/local/src/emacs-cvs/lib-src:$(dir $(EMACS))../lib-src:$$PATH etags --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

.PHONY: clean all dist plt tar check svnup
