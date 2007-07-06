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
	mzscheme -qu run-tests.ss

.PHONY: clean install uninstall check test install-test
