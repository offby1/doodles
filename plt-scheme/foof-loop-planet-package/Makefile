plt-file := foof-loop-planet-plt-file.plt
owner := offby1
$(plt-file): info.ss doc.txt foof-loop syn-param test.ss wrapper.ss
	planet --create-archive . 

clean:
	-rm $(plt-file)

install: $(plt-file)
	planet --file $@ $(owner) 1 0

uninstall:
	planet  --erase $(owner) $(plt-file) 1 0
.PHONY: clean install
