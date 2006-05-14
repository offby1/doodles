bytecode := anagrams.scshvm
$(bytecode): $(wildcard *.scm)
	./compile.scm

check: $(bytecode)
	time scshvm -h 11659948 -i ./$^ "Ernest"

clean:
	rm -f $(bytecode)

.PHONY: check clean
