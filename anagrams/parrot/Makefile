bag-check: bag.pbc
	./parrot $<

dict-check: dict.pbc bag.pbc
	time ./parrot --profile $<

check: bag-check dict-check

clean:
	-rm -f *.pbc

%.pbc: %.pir
	./parrot -o $@ $^

.PHONY: bag-check dict-check check clean
