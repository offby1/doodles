basename := $(shell echo rudybot-$$(svnversion -n . | sed s,:,-,))
plt-file := $(basename).plt
tarball  := $(basename).tar.gz

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

clean:
	rm -rf $(plt-file) $(tarball)

.PHONY: clean all plt tar
