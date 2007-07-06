plt-file := foof-loop.plt
owner := offby1
version := 1 0

sources := foof-loop syn-param
tests   := test test-foof-loop test-parameters

$(plt-file): $(wildcard *.ss) doc.txt $(sources) $(tests)
	planet --create-archive . 

clean:
	-rm $(plt-file)

# planet --file gacks if it's already installed, so we uninstall first.
install: $(plt-file)
	-$(MAKE) uninstall
	planet --file $^ $(owner) $(version)

uninstall:
	planet  --erase $(owner) $(plt-file) $(version)

installcheck: check install
	./test-install.ss

check test: 
	mzscheme -qu run-tests.ss

.PHONY: clean install uninstall check test installcheck
