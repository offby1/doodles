plt-file := foof-loop.plt
owner := offby1
$(plt-file): $(wildcard *.ss *.scm) doc.txt foof-loop syn-param
	planet --create-archive . 

clean:
	-rm $(plt-file)

# planet --file gacks if it's already installed, so we uninstall first.
install: $(plt-file)
	-$(MAKE) uninstall
	planet --file $^ $(owner) 1 0

uninstall:
	planet  --erase $(owner) $(plt-file) 1 0

install-test: check install
	./test-install.ss

check test: 
	mzscheme -qmv --require foof-loop.ss --case-insens -L 9.ss srfi -L 8.ss srfi -f test.scm  -f test-foof-loop.scm -f test-parameters.scm -e "(run-test-suite loop-tests)" 

.PHONY: clean install uninstall check test install-test
