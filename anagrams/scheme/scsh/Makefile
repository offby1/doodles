bytecode := anagrams.scshvm
vm := /usr/local/bin/scshvm

$(bytecode): $(vm) $(wildcard *.scm)
	./compile.scm

$(vm):
	@echo $@ is missing.
	@echo sudo ln -s /usr/local/lib/scsh/scshvm $@
	@echo might do it.
	@false

check: $(bytecode)
	time scshvm -h 11659948 -i ./$^ "Ernest"

clean:
	rm -f $(bytecode)

.PHONY: check clean
