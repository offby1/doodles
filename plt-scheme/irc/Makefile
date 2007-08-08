basename := rudybot
plt-file := $(basename).plt

all $(plt-file):
	tempdir=`mktemp -d botXXXXXX` ; \
	expdir=$${tempdir}/$(basename) ; \
	svn export . $${expdir} && \
	planet --create-archive $${expdir} && \
	rm -rfv $${tempdir}

clean:
	rm -rf $(plt-file)

.PHONY: clean all
