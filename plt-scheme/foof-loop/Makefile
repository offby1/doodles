plt-file := foof-loop.plt
owner := offby1
$(plt-file): info.ss doc.txt foof-loop syn-param test.ss foof-loop.ss
	planet --create-archive . 

clean:
	-rm $(plt-file)

# planet --file gacks if it's already installed, so we uninstall first.
install: $(plt-file)
	-$(MAKE) uninstall
	planet --file $^ $(owner) 1 0

uninstall:
	planet  --erase $(owner) $(plt-file) 1 0

check test: install
	./test.ss

.PHONY: clean install uninstall check test
