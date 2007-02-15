bag-check: bag.pbc
	./parrot $<

dict-check: dict.pbc bag.pbc
	./parrot $<

check: bag-check dict-check

%.pbc: %.pir
	./parrot -o $@ $^

.PHONY: bag-check dict-check check
