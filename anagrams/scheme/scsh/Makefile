yowza: $(wildcard *.scm)
	./compile.scm

check: yowza
	time scshvm -h 11659948 -i ./$^ "Ernest hemingway"

clean:
	rm -f yowza

.PHONY: check clean
