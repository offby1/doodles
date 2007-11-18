exe = flickr-thingy.exe

all: package/$(exe)

$(exe): $(wildcard *.ss)
	mzc --gui-exe $@ gui.ss 

package/$(exe): $(exe)
	mzc --exe-dir $(dirname $@) $^

check: $(exe)
	PATH=.:$$PATH $^

.PHONY: check
