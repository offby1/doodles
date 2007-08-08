basename := $(shell echo rudybot-$$(svnversion -n . | sed s,:,-,))
plt-file := $(basename).plt

all: $(plt-file)

$(plt-file):
	tempdir=`mktemp -d botXXXXXX` ; \
	expdir=$${tempdir}/$(basename) ; \
	svn export . $${expdir} && \
	planet --create-archive $${expdir} && \
	rm -rf $${tempdir}

clean:
	rm -rf $(plt-file)

.PHONY: clean all
