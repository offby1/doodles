basename := $(shell echo rudybot-$$(svnversion -n . | sed s,:,-,))
plt-file := $(basename).plt
tarball  := $(basename).tar.gz
distdir  := $(HOME)/public_html/bot

all: plt tar
plt: $(plt-file)
tar: $(tarball)

define make-export-dir
tempdir=`mktemp -d botXXXXXX` ; \
expdir=$${tempdir}/$(basename) ; \
svn export . $${expdir}
endef

define cleanup
rm -rf $${tempdir}
endef

$(plt-file):
	$(make-export-dir) && \
	planet --create-archive $${expdir} && \
	$(cleanup)

$(tarball):
	$(make-export-dir) && \
	tar -C $${tempdir} --create --gzip --file=$@ $(basename) && \
	$(cleanup)

install: $(tarball)
	mv -v $^ $(distdir)
	ln -svf $(distdir)/$^ $(distdir)/rudybot.tar.gz

check:
	./run-all-tests.ss

clean:
	rm -rf $(plt-file) $(tarball)

tags: TAGS

# This tags file is almost useless -- it just contains the names of
# the scheme files, but doesn't contain any actual tags.  But it's
# useful for emacs commands that work on all the files in it, such as
# tags-query-replace.
TAGS: $(wildcard *.ss)
	etags $^

.PHONY: clean all plt tar check
