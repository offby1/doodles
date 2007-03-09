p=/usr/local/src/langs/my-parrot-branch/parrot

sources=bag dict anagrams
pbcs=$(addsuffix, .pbc, $(sources))

all: $(pbcs)

bag-check: bag.pbc
	$(p) $<

dict-check: dict.pbc bag.pbc
	time $(p)  $<

test: check

check: anagrams.pbc dict.pbc bag.pbc
	time $(p)  $< 'est Hemingway'

clean:
	-rm -f *.pbc

%.pbc: %.pir
	$(p) -o $@ $^

%.pasm: %.pir
	$(p) -o $@ $^

.PHONY: bag-check dict-check check clean test
