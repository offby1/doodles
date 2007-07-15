# at Amazon, you'd invoke this makefile like this:

# make mzscheme="/apollo/bin/env /apollo/env/hanchrow-PLT/bin/mzscheme" planet="/apollo/bin/env /apollo/env/hanchrow-PLT/bin/planet"

mzscheme := mzscheme
planet   := planet

plt-file := foof-loop.plt
owner := offby1
version := 8 0

sources := foof-loop syn-param
tests   := test test-foof-loop test-parameters

# planet --create-archive rather stupidly adds every file it finds,
# rather than letting me specify the files I want to add.  So to keep
# crap outta the archive, I create a fresh copy, create the archive
# from that copy, then nuke the copy.

# Oh, and it doesn't work to make x/foof-loop a prerequisite of
# .INTERMEDIATE, at least in GNU Make 3.81, because it tries to remove
# the thing with "rm" instead of "rm -r".  Daft punter.

$(plt-file): x/foof-loop
	$(planet) --create-archive $^
	rm -rf x

x/foof-loop:
	mkdir -vp $$(dirname $@)
	svn export . $@

clean:
	-rm $(plt-file)

# planet --file gacks if it's already installed, so we uninstall first.

# TODO -- find a way of examining the installed thingy, and seeing if
# it's actually older than the current plt-file.
install: $(plt-file)
	-$(MAKE) uninstall
	$(planet) --file $^ $(owner) $(version)

uninstall:
	$(planet)  --erase $(owner) $(plt-file) $(version)

installcheck: check install
	$(mzscheme) -qu ./test-install.ss

check: 
	$(mzscheme) -qu run-tests.ss

.PHONY: clean install uninstall check installcheck
