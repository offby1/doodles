exe = flickr-thingy.exe
pdir = package
all: $(pdir)/$(exe)

$(exe): $(wildcard *.ss)
	mzc --gui-exe $@ gui.ss 

$(pdir)/$(exe): $(exe)
	mzc --exe-dir $(pdir) $^

check: $(exe)
	PATH=.:$$PATH $^

.PHONY: check
