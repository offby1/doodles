p=/usr/local/src/langs/parrot-svn/parrot

bag-check: bag.pbc
	$(p) $<

dict-check: dict.pbc bag.pbc
	time $(p)  $<

check: anagrams.pbc dict.pbc bag.pbc
	time $(p)  $< aola

clean:
	-rm -f *.pbc

%.pbc: %.pir
	$(p) -o $@ $^

%.pasm: %.pir
	$(p) -o $@ $^

.PHONY: bag-check dict-check check clean
