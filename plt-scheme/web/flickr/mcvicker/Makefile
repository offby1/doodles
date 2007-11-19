exe = flickr-thingy.exe
pdir = flickr-thingy
zip = flickr-thingy.zip

all: $(zip)

$(zip): $(pdir)/$(exe)
	zip -ur $@ $(pdir)

$(exe): $(wildcard *.ss)
	mzc --gui-exe $@ gui.ss 

$(pdir)/$(exe): $(exe)
	mzc --exe-dir $(pdir) $^

check: $(exe)
	PATH=.:$$PATH $^

.PHONY: check
