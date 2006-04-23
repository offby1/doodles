yowza: $(wildcard *.scm)
	./top.scm

check: yowza
	time scshvm -h 11659948 -i ./$^ hemingway

.PHONY: check
